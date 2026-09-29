package com.webforj.devtools.craftforj.source.structure;

import com.github.javaparser.Position;
import com.github.javaparser.ast.Modifier;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.SimpleName;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.nodeTypes.NodeWithArguments;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.DoStmt;
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForEachStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.WhileStmt;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.resolver.MethodResolver;
import com.webforj.devtools.craftforj.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.source.resolver.TypeResolver;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

/**
 * Takes components out of a file.
 *
 * <p>
 * The cutter detaches a component from what it is handed to and removes every statement of its
 * slice and of the slices below it. What is left in the file never reads a variable that is gone, a
 * component something else still reads is detached and keeps its declaration, and a component that
 * can neither go nor be detached is refused.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class DetachWriter {

  private final ParentReference parent;
  private final AttachWriter attach;

  /**
   * Creates a cutter.
   *
   * @param parent the parent the component is attached to, or {@code null} when it is not known
   * @param attach the attach point on that parent, or {@code null} when it is not known
   */
  DetachWriter(ParentReference parent, AttachWriter attach) {
    this.parent = parent;
    this.attach = attach;
  }

  /**
   * Detaches a component from its parent and leaves its declaration alone.
   *
   * @param root the component slice
   * @param withItemCalls {@code true} to also remove the calls that set the component's place in
   *        the parent, which hold only while it stays a child of that parent
   *
   * @return the statements of the item calls that were kept, in source order
   */
  List<Statement> detach(ComponentSlice root, boolean withItemCalls) {
    List<Statement> kept = new ArrayList<>();
    for (Expression argument : new ArrayList<>(root.getArguments())) {
      if (attach.isAttachment(argument)) {
        detachArgument(argument);
      } else if (isItemCall(argument)) {
        if (withItemCalls) {
          detachArgument(argument);
        } else {
          kept.add(AttachWriter.findBlockStatement(argument));
        }
      }
    }

    return kept;
  }

  /**
   * Removes the given components from the file.
   *
   * @param slices the component slice first, the slices of the components below it after
   * @param pivot the place of an attach call that is already gone, or {@code null} to read it from
   *        the file
   * @param keepReferenced {@code true} to leave what the rest of the file still reads declared and
   *        only detach it, {@code false} to refuse it
   *
   * @return what was removed, as copies ready to be written somewhere else
   *
   * @throws SourceModificationException when the component can neither be removed nor detached
   */
  CutResult cut(List<ComponentSlice> slices, Position pivot, boolean keepReferenced) {
    ComponentSlice root = slices.get(0);
    requireSingleRun(root);

    List<ComponentSlice> leaving = findLeaving(slices, keepReferenced);
    List<Node> removed = collectGone(leaving);
    Node retained = leaving.contains(root) ? null : findRetained(root, List.of(), keepReferenced);
    if (retained != null) {
      if (!keepReferenced) {
        throw refuse(root, retained);
      }

      return unhook(root, retained);
    }

    List<Expression> detached = new ArrayList<>();
    for (ComponentSlice slice : leaving) {
      slice.getArguments().stream()
          .filter(argument -> isInFile(argument) && !isInside(argument, removed))
          .forEach(detached::add);
    }

    Position attachedAt = pivot != null ? pivot
        : detached.stream().filter(root.getArguments()::contains).findFirst()
            .flatMap(Node::getRange).map(range -> range.begin).orElse(null);
    CutResult result = copyRemoved(root, collectRemoved(leaving), attachedAt);
    detached.forEach(this::detachArgument);
    collectRemoved(leaving).forEach(DetachWriter::removeNode);

    return result;
  }

  /**
   * Renames the variables of the given slices everywhere their slices read them.
   *
   * <p>
   * The rename runs on the nodes still in the file, so the printer keeps their text in step and a
   * copy taken afterwards prints the new names in the formatting the developer wrote.
   * </p>
   *
   * @param slices the slices to rename
   * @param renames the new name of each variable that changes
   */
  static void rename(List<ComponentSlice> slices, Map<String, String> renames) {
    if (renames.isEmpty()) {
      return;
    }

    for (Node node : collectRemoved(slices)) {
      for (SimpleName name : node.findAll(SimpleName.class)) {
        Node holder = name.getParentNode().orElse(null);
        boolean variable = holder instanceof NameExpr || holder instanceof VariableDeclarator
            || (holder instanceof FieldAccessExpr access && access.getScope().isThisExpr());
        if (variable && renames.containsKey(name.getIdentifier())) {
          name.setIdentifier(renames.get(name.getIdentifier()));
        }
      }
    }
  }

  @SuppressWarnings("unchecked")
  static <T extends Node> T copy(T node) {
    return (T) node.clone();
  }

  // A component below the one that leaves goes with it, unless the rest of the file still reads
  // it. Keeping one can leave another one read, so the check runs until nothing changes.
  private List<ComponentSlice> findLeaving(List<ComponentSlice> slices, boolean keepReferenced) {
    ComponentSlice root = slices.get(0);
    List<ComponentSlice> leaving = new ArrayList<>(slices);
    if (keepReferenced) {
      leaving.removeIf(slice -> slice != root && !isSingleRun(slice));
    } else {
      slices.forEach(DetachWriter::requireSingleRun);
    }

    ComponentSlice kept = findKept(leaving, root, keepReferenced);
    while (kept != null) {
      leaving.remove(kept);
      kept = findKept(leaving, root, keepReferenced);
    }

    return leaving;
  }

  // The first component that leaves while the rest of the file still holds it
  private ComponentSlice findKept(List<ComponentSlice> leaving, ComponentSlice root,
      boolean keepReferenced) {
    List<Node> removed = collectGone(leaving);
    for (ComponentSlice slice : leaving) {
      Node retained = findRetained(slice, removed, keepReferenced);
      if (retained != null) {
        if (slice != root && !keepReferenced) {
          throw refuse(slice, retained);
        }

        return slice;
      }
    }

    return null;
  }

  // The first use that holds a component in the file, a field other classes can reach counts
  // as one
  private Node findRetained(ComponentSlice slice, List<Node> removed, boolean keepReferenced) {
    for (Node reference : slice.getUnexplained()) {
      if (isInFile(reference) && !isInside(reference, removed)) {
        return reference;
      }
    }

    for (Expression argument : slice.getArguments()) {
      if (!isInFile(argument) || isInside(argument, removed)) {
        continue;
      }

      boolean detachable = keepReferenced ? isDetachable(argument) : isKnownAttachment(argument);
      if (!detachable) {
        return argument;
      }
    }

    VariableDeclarator declarator = slice.getDeclarator();
    boolean reachable = keepReferenced && declarator != null
        && declarator.getParentNode().orElse(null) instanceof FieldDeclaration field
        && !field.isPrivate();

    return reachable ? declarator : null;
  }

  private CutResult unhook(ComponentSlice root, Node retained) {
    List<Expression> attachments = root.getArguments().stream().filter(this::isDetachable).toList();
    if (attachments.isEmpty() || root.getInlineCreation() != null) {
      throw refuse(root, retained);
    }

    attachments.forEach(this::detachArgument);

    return new CutResult();
  }

  private boolean isKnownAttachment(Expression argument) {
    return attach != null && (attach.isAttachment(argument) || isItemCall(argument));
  }

  private boolean isItemCall(Expression argument) {
    return parent != null && !parent.isImplicit()
        && ComponentSlice.getReceiver(argument) instanceof MethodCallExpr call
        && parent.isCallOnParent(call);
  }

  private boolean isDetachable(Expression argument) {
    if (isKnownAttachment(argument)) {
      return true;
    }

    NodeWithArguments<?> receiver = (NodeWithArguments<?>) ComponentSlice.getReceiver(argument);
    int index = AstFinder.indexOfSame(receiver.getArguments(), argument);
    // A creation, and a constructor that hands over to another one, build the object they name
    if (!(receiver instanceof MethodCallExpr call)) {
      Class<?> built = receiver instanceof ObjectCreationExpr creation
          ? TypeResolver.resolve(creation.getType(), creation)
          : TypeResolver.resolveEnclosing((Node) receiver);

      return TypeResolver.isComponent(built) && (MethodResolver.isComponentVarargs(receiver, index)
          || MethodResolver.hasOverloadWithout(receiver, index));
    }

    if (!TypeResolver.isComponent(MethodResolver.resolveReceiver(call))
        || MethodResolver.findMethods(call).isEmpty()
        || MethodResolver.isProjectMethod(call, DetachWriter::isProjectClass)) {
      return false;
    }

    if (MethodResolver.isComponentVarargs(receiver, index)) {
      return true;
    }

    return isStatement(call) || isLink(call) ? !handsOverAnother(call, index)
        : MethodResolver.hasOverloadWithout(receiver, index);
  }

  private static boolean isProjectClass(Class<?> type) {
    return SourceFileResolver.resolve(type.getName(), SourceFileResolver.ALL_EXTENSIONS) != null;
  }

  // Taking the call away would detach the other component as well
  private static boolean handsOverAnother(MethodCallExpr call, int index) {
    for (int other = 0; other < call.getArguments().size(); other++) {
      Class<?> type = other == index ? null : TypeResolver.resolveType(call.getArgument(other));
      if (type != null && TypeResolver.isComponent(type)) {
        return true;
      }
    }

    return false;
  }

  private static boolean isStatement(MethodCallExpr call) {
    return call.getParentNode().orElse(null) instanceof ExpressionStmt;
  }

  // A call leaves a chain when its receiver can stand in for what the call returned
  private static boolean isLink(MethodCallExpr call) {
    Expression scope = call.getScope().orElse(null);
    if (isStatement(call) || scope == null || MethodResolver.isStatic(call)) {
      return false;
    }

    Class<?> returned = MethodResolver.resolveReturnType(call);
    Class<?> receiver = TypeResolver.resolveType(scope);

    return returned == null || receiver == null || returned.isAssignableFrom(receiver);
  }

  // What leaves the file, the removed code and what is written inside a creation that is taken
  // out of the call it is handed to
  private static List<Node> collectGone(List<ComponentSlice> slices) {
    List<Node> gone = collectRemoved(slices);
    for (ComponentSlice slice : slices) {
      if (slice.getInlineCreation() != null) {
        gone.addAll(slice.getInlineCreation().getChildNodes());
      }
    }

    return gone;
  }

  private static List<Node> collectRemoved(List<ComponentSlice> slices) {
    List<Node> removed = new ArrayList<>();
    for (ComponentSlice slice : slices) {
      VariableDeclarator declarator = slice.getDeclarator();
      if (declarator != null) {
        Node declaration = declarator.getParentNode().orElseThrow();
        boolean shared =
            declaration instanceof FieldDeclaration field ? field.getVariables().size() > 1
                : ((VariableDeclarationExpr) declaration).getVariables().size() > 1;
        if (shared) {
          removed.add(declarator);
        } else {
          removed.add(declaration instanceof FieldDeclaration ? declaration
              : ComponentSlice.findStatement(declaration));
        }
      }

      slice.getStatements().stream().filter(statement -> !removed.contains(statement))
          .forEach(removed::add);
    }

    return removed;
  }

  private static CutResult copyRemoved(ComponentSlice root, List<Node> removed, Position pivot) {
    CutResult result = new CutResult();
    List<Node> ordered = new ArrayList<>(removed);
    if (root.getInlineCreation() != null) {
      // What the inline component holds may be declared in statements of its own, the statement
      // that is the inline component itself is the component
      result.setInlineCreation(copy(root.getInlineCreation()));
      ordered.removeIf(node -> node.isAncestorOf(root.getInlineCreation()));
    }

    ordered.sort(Comparator.comparing(node -> node.getRange().map(range -> range.begin)
        .orElse(new Position(Integer.MAX_VALUE, 0))));

    for (Node node : ordered) {
      if (node instanceof FieldDeclaration field) {
        result.getFields().add(copy(field));
      } else if (node instanceof VariableDeclarator declarator) {
        copyShared(declarator, result);
      } else if (node instanceof Statement statement) {
        boolean after = pivot != null
            && statement.getRange().map(range -> range.begin.isAfter(pivot)).orElse(false);
        (after ? result.getTrailing() : result.getLeading()).add(copy(statement));
      }
    }

    return result;
  }

  // A declarator sharing its declaration with others leaves as a declaration of its own
  private static void copyShared(VariableDeclarator declarator, CutResult result) {
    Node declaration = declarator.getParentNode().orElseThrow();
    if (declaration instanceof FieldDeclaration field) {
      NodeList<Modifier> modifiers = new NodeList<>();
      field.getModifiers().forEach(modifier -> modifiers.add(modifier.clone()));
      result.getFields().add(new FieldDeclaration(modifiers, copy(declarator)));
    } else {
      result.getLeading().add(new ExpressionStmt(new VariableDeclarationExpr(copy(declarator))));
    }
  }

  private void detachArgument(Expression argument) {
    NodeWithArguments<?> receiver = (NodeWithArguments<?>) ComponentSlice.getReceiver(argument);
    int index = AstFinder.indexOfSame(receiver.getArguments(), argument);
    boolean created = !(receiver instanceof MethodCallExpr)
        || parent != null && parent.isCreation((Node) receiver);

    if (created) {
      if (!isVarargs(receiver, index) && !MethodResolver.hasOverloadWithout(receiver, index)) {
        throw new SourceModificationException("The creation of " + describeReceiver(receiver)
            + " at line " + argument.getRange().map(range -> range.begin.line).orElse(0)
            + " takes a fixed set of arguments, the component cannot be taken out of it");
      }

      argument.remove();

      return;
    }

    MethodCallExpr call = (MethodCallExpr) receiver;
    boolean sharesTail = false;
    for (int other = 0; other < call.getArguments().size(); other++) {
      sharesTail |= other != index && isVarargs(receiver, other);
    }

    boolean varargs = isVarargs(receiver, index);
    if (varargs && sharesTail) {
      argument.remove();
    } else if (isStatement(call) || isLink(call)) {
      removeCall(call);
    } else if (varargs || MethodResolver.hasOverloadWithout(receiver, index)) {
      argument.remove();
    } else {
      throw new SourceModificationException("Cannot take the call at line "
          + call.getRange().map(range -> range.begin.line).orElse(0) + " out of its statement");
    }
  }

  // The parent named by the attach point is known by its type even where the file itself gives
  // the type of the receiver away no more
  private boolean isVarargs(NodeWithArguments<?> receiver, int index) {
    if (MethodResolver.isComponentVarargs(receiver, index)) {
      return true;
    }

    if (parent == null) {
      return false;
    }

    Node node = (Node) receiver;
    if (parent.isCreation(node)) {
      String method = receiver instanceof MethodCallExpr call ? call.getNameAsString() : null;

      return MethodResolver.isComponentVarargs(
          TypeResolver.resolve(parent.getCreationType(node), node), method, index);
    }

    return receiver instanceof MethodCallExpr call && parent.isCallOnParent(call)
        && MethodResolver.isComponentVarargs(TypeResolver.resolve(parent.getTypeName(), node),
            call.getNameAsString(), index);
  }

  private String describeReceiver(NodeWithArguments<?> receiver) {
    if (parent != null && parent.isCreation((Node) receiver)) {
      return ComponentSlice.describe(parent.getLocation());
    }

    if (receiver instanceof ExplicitConstructorInvocationStmt invocation) {
      return invocation.isThis() ? "this" : "super";
    }

    return receiver instanceof ObjectCreationExpr creation ? creation.getType().getNameAsString()
        : ((MethodCallExpr) receiver).getNameAsString();
  }

  private static void removeCall(MethodCallExpr call) {
    Node holder = call.getParentNode().orElseThrow();
    Expression callScope = call.getScope().orElse(null);

    if (holder instanceof ExpressionStmt statement && !hasSideEffect(callScope)) {
      removeNode(statement);

      return;
    }

    if (callScope == null) {
      throw new SourceModificationException("Cannot take the call at line "
          + call.getRange().map(range -> range.begin.line).orElse(0) + " out of its statement");
    }

    call.replace(callScope.clone());
  }

  private static boolean hasSideEffect(Expression expression) {
    Expression current = expression;
    while (current instanceof MethodCallExpr link) {
      boolean accessor = link.getArguments().isEmpty()
          && (link.getNameAsString().startsWith("get") || link.getNameAsString().startsWith("is"));
      if (!accessor) {
        return true;
      }

      current = link.getScope().orElse(null);
    }

    return current instanceof ObjectCreationExpr;
  }

  // A statement that is the whole body of a branch, of a loop or of a lambda leaves an empty body
  // behind, the code around it needs one
  private static void removeNode(Node node) {
    boolean body = node instanceof Statement statement && !ComponentSlice.isListed(statement);
    boolean removed = body ? node.replace(new BlockStmt()) : node.remove();
    if (!removed) {
      throw new SourceModificationException("Cannot remove the code at line "
          + node.getRange().map(range -> range.begin.line).orElse(0));
    }
  }

  // One source line inside a loop or a lambda builds a component every time it runs
  private static void requireSingleRun(ComponentSlice slice) {
    if (!isSingleRun(slice)) {
      throw new SourceModificationException(ComponentSlice.describe(slice.getLocation())
          + " is created inside a loop or a lambda, one line builds several components");
    }
  }

  private static boolean isSingleRun(ComponentSlice slice) {
    // What the running application built is known for a component that was resolved from it
    if (slice.getLocation().getSite() != null) {
      return true;
    }

    Node creation =
        slice.getDeclarator() != null ? slice.getDeclarator() : slice.getInlineCreation();
    Node current = creation.getParentNode().orElse(null);
    while (current != null) {
      if (current instanceof ForStmt || current instanceof ForEachStmt
          || current instanceof WhileStmt || current instanceof DoStmt
          || current instanceof LambdaExpr) {
        return false;
      }

      current = current.getParentNode().orElse(null);
    }

    return true;
  }

  // A node an earlier step took out of the file holds nothing any more
  private static boolean isInFile(Node node) {
    return node.findCompilationUnit().isPresent();
  }

  private static boolean isInside(Node node, List<Node> removed) {
    return removed.stream()
        .anyMatch(candidate -> candidate == node || candidate.isAncestorOf(node));
  }

  private static SourceModificationException refuse(ComponentSlice slice, Node reference) {
    if (reference instanceof VariableDeclarator declarator) {
      return new SourceModificationException(ComponentSlice.describe(slice.getLocation())
          + " is a field other classes can reach at line "
          + declarator.getRange().map(range -> range.begin.line).orElse(0)
          + ", nothing in this file attaches it");
    }

    Node holder = reference;
    Node outer = holder.getParentNode().orElse(null);
    while (outer != null
        && !(holder instanceof Statement statement && ComponentSlice.isListed(statement))) {
      holder = outer;
      outer = holder.getParentNode().orElse(null);
    }

    String code = holder instanceof Statement ? holder.toString()
        : reference.getParentNode().map(Node::toString).orElse("");

    return new SourceModificationException(
        ComponentSlice.describe(slice.getLocation()) + " is still used at line "
            + reference.getRange().map(range -> range.begin.line).orElse(0) + ", " + code.strip());
  }
}
