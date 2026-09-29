package com.webforj.devtools.craftforj.source.structure;

import com.github.javaparser.Position;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Modifier;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.CallableDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.comments.Comment;
import com.github.javaparser.ast.expr.ConditionalExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.nodeTypes.NodeWithArguments;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.DoStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForEachStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.IfStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntry;
import com.github.javaparser.ast.stmt.WhileStmt;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.parser.AstModifier;
import com.webforj.devtools.craftforj.source.resolver.MethodResolver;
import com.webforj.devtools.craftforj.source.resolver.TypeResolver;
import com.webforj.devtools.craftforj.source.resolver.VariableResolver;
import com.webforj.devtools.craftforj.source.structure.model.AttachPoint;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * Places a child on a parent at an attach point.
 *
 * <p>
 * The child lands next to its sibling in the same shape the sibling is attached in. It becomes one
 * more argument only when the loaded signature proves the call is a varargs call of components,
 * otherwise it gets a call of its own. A place the writer cannot keep in order is refused.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class AttachWriter {

  private final CompilationUnit cu;
  private final ParentReference parent;
  private final AttachPoint point;

  AttachWriter(CompilationUnit cu, ParentReference parent, AttachPoint point) {
    this.cu = cu;
    this.parent = parent;
    this.point = point;
  }

  /**
   * Finds the argument that attaches a component to the parent at this point.
   *
   * @param slice the component slice
   *
   * @return the attaching argument
   *
   * @throws SourceModificationException when the component is attached nowhere or more than once
   */
  Expression findAttachment(ComponentSlice slice) {
    List<Expression> attachments =
        slice.getArguments().stream().filter(this::isAttachment).toList();

    if (attachments.size() != 1) {
      throw new SourceModificationException(ComponentSlice.describe(slice.getLocation())
          + (attachments.isEmpty() ? " is not attached to " : " is attached more than once to ")
          + ComponentSlice.describe(parent.getLocation()) + " with " + point.getMethodNames()
          + " in " + ComponentSlice.getFileName(parent.getLocation()));
    }

    return attachments.get(0);
  }

  /**
   * Checks whether an argument attaches its component to the parent at this point.
   *
   * @param argument an argument of a slice
   *
   * @return {@code true} for a creation of the parent and for a call of one of the point's methods
   */
  boolean isAttachment(Expression argument) {
    Node receiver = ComponentSlice.getReceiver(argument);

    return parent.isCreation(receiver) || (receiver instanceof MethodCallExpr call
        && point.getMethodNames().contains(call.getNameAsString()) && parent.isCallOnParent(call));
  }

  /**
   * Places the child and the statements that travel with it.
   *
   * @param child the expression naming or creating the child
   * @param leading the statements that must come before the attach call
   * @param trailing the statements that must come after the attach call
   *
   * @return the statement that now attaches the child
   */
  Statement place(Expression child, List<Statement> leading, List<Statement> trailing) {
    Statement attached = point.getAnchor() == null ? append(child, leading)
        : placeNextTo(ComponentSlice.of(cu, point.getAnchor()), child, leading);

    insertAfter(attached, trailing);

    return attached;
  }

  /**
   * Places a call of its own on the parent, after the last attach call, a part added through a
   * call.
   *
   * @param arguments the arguments of the call
   *
   * @return the statement that now holds the call
   */
  Statement placeCall(List<Expression> arguments) {
    Statement attach = new ExpressionStmt(parent.createCall(point.getMethodName(), arguments));
    List<Statement> run = new ArrayList<>(List.of(attach));
    List<MethodCallExpr> calls = findAttachCalls();

    if (calls.isEmpty()) {
      insertIntoEmptySlot(run);
    } else {
      insertAfter(getBodyStatement(calls.get(calls.size() - 1)), run);
    }

    return attach;
  }

  /**
   * Adds a statement to a block and keeps the comment written above it.
   *
   * <p>
   * The printer drops the comment of a statement it is handed, it only prints a comment set on a
   * statement that is already in the file.
   * </p>
   *
   * @param block the block
   * @param index the position in the block
   * @param statement the statement
   */
  static void addStatement(BlockStmt block, int index, Statement statement) {
    Comment comment = statement.getComment().orElse(null);
    statement.removeComment();
    block.addStatement(index, statement);
    if (comment != null) {
      statement.setComment(comment.clone());
    }
  }

  /**
   * Gets the statement directly inside a block that holds the given node.
   *
   * @param node the node
   *
   * @return the statement
   *
   * @throws SourceModificationException when the node sits in a statement without braces
   */
  static Statement findBlockStatement(Node node) {
    Node current = node;
    do {
      if (current instanceof Statement statement
          && statement.getParentNode().orElse(null) instanceof BlockStmt) {
        return statement;
      }

      current = current.getParentNode().orElse(null);
    } while (current != null);

    throw new SourceModificationException("The code at line "
        + node.getRange().map(range -> range.begin.line).orElse(0) + " is not inside a block");
  }

  /**
   * Refuses a node that only runs sometimes or runs several times.
   *
   * @param node the node to check
   * @param what the words naming the node in the message
   *
   * @throws SourceModificationException when the node sits in a branch, a loop or a lambda
   */
  static void requireUnconditional(Node node, String what) {
    if (!isUnconditional(node)) {
      throw new SourceModificationException(
          what + " at line " + node.getRange().map(range -> range.begin.line).orElse(0)
              + " sits inside a branch, a loop or a lambda");
    }
  }

  private Statement placeNextTo(ComponentSlice anchor, Expression child, List<Statement> leading) {
    Expression attachment = findAttachment(anchor);
    String anchorName = ComponentSlice.describe(anchor.getLocation());
    requireUnconditional(attachment, "The attach call of " + anchorName);

    NodeWithArguments<?> receiver = (NodeWithArguments<?>) ComponentSlice.getReceiver(attachment);
    Statement statement = findBlockStatement(attachment);
    int index = receiver.getArguments().indexOf(attachment);

    if (isVarargsArgument(receiver, index) && !isOneCallPerChild(receiver)) {
      insertBefore(findLeadingPosition(anchor, statement, false), leading);
      receiver.getArguments().add(point.isBefore() ? index : index + 1, child);

      return statement;
    }

    if (parent.isCreation((Node) receiver)) {
      throw new SourceModificationException("Cannot place a component next to " + anchorName
          + ", it is handed to the creation of " + ComponentSlice.describe(parent.getLocation())
          + " which takes a fixed set of arguments");
    }

    if (findAttachCalls().stream().filter(statement::isAncestorOf).count() > 1) {
      throw new SourceModificationException("Cannot place a component next to " + anchorName
          + ", its statement attaches several components in one chain");
    }

    Statement attach = new ExpressionStmt(parent.createCall(point.getMethodName(), child));
    Statement current = findBlockStatement(attachment);
    BlockStmt block = (BlockStmt) current.getParentNode().orElseThrow();
    if (point.isBefore()) {
      Statement position = findLeadingPosition(anchor, current, true);
      block.addStatement(block.getStatements().indexOf(position), attach);
    } else {
      block.addStatement(block.getStatements().indexOf(current) + 1, attach);
    }

    insertBefore(attach, leading);

    return attach;
  }

  private Statement append(Expression child, List<Statement> leading) {
    List<MethodCallExpr> calls = findAttachCalls();

    if (calls.size() == 1 && isUnconditional(calls.get(0))) {
      MethodCallExpr only = calls.get(0);
      if (isVarargsArgument(only, only.getArguments().size())) {
        Statement statement = findBlockStatement(only);
        insertBefore(statement, leading);
        only.addArgument(child);

        return statement;
      }
    }

    Statement attach = new ExpressionStmt(parent.createCall(point.getMethodName(), child));
    List<Statement> run = new ArrayList<>(leading);
    run.add(attach);

    if (calls.isEmpty()) {
      insertIntoEmptySlot(run);
    } else {
      Statement last = getBodyStatement(calls.get(calls.size() - 1));
      insertAfter(last, run);
    }

    return attach;
  }

  private void insertIntoEmptySlot(List<Statement> run) {
    ClassOrInterfaceDeclaration type = findType();
    Statement slot = findSlot(type);
    if (slot != null) {
      insertAfter(slot, run);

      return;
    }

    if (!type.getConstructors().isEmpty()) {
      BlockStmt body = type.getConstructors().get(0).getBody();
      run.forEach(body::addStatement);

      return;
    }

    // The printer loses statements added to a constructor that is already in the class
    ConstructorDeclaration constructor = new ConstructorDeclaration(
        new NodeList<>(Modifier.publicModifier()), type.getNameAsString());
    run.forEach(constructor.getBody()::addStatement);
    List<FieldDeclaration> fields = type.getFields();
    if (fields.isEmpty()) {
      type.getMembers().addFirst(constructor);
    } else {
      type.getMembers().addAfter(constructor, fields.get(fields.size() - 1));
    }
  }

  private ClassOrInterfaceDeclaration findType() {
    VariableDeclarator declarator = parent.getDeclarator();
    if (declarator != null) {
      return AstFinder.findAncestor(declarator, ClassOrInterfaceDeclaration.class).orElseThrow();
    }

    return cu.findFirst(ClassOrInterfaceDeclaration.class)
        .orElseThrow(() -> new SourceModificationException(
            "No class found in " + ComponentSlice.getFileName(parent.getLocation())));
  }

  // The first attach call follows the last call made on the parent, and where none is made the
  // statement that declares the parent or assigns it
  private Statement findSlot(ClassOrInterfaceDeclaration type) {
    VariableDeclarator declarator = parent.getDeclarator();
    Statement local = declarator == null ? null
        : AstFinder.findAncestor(declarator, Statement.class).orElse(null);
    if (local != null) {
      Statement declaration = findBlockStatement(local);
      BlockStmt block = (BlockStmt) declaration.getParentNode().orElseThrow();
      int calls = AstModifier.findInsertionPointForVariable(block, parent.getVariableName());

      return block.getStatement(calls >= 0 ? calls : block.getStatements().indexOf(declaration));
    }

    List<BlockStmt> bodies = new ArrayList<>();
    type.getConstructors().stream().map(ConstructorDeclaration::getBody).forEach(bodies::add);
    type.getMethods().stream().map(MethodDeclaration::getBody).flatMap(Optional::stream)
        .forEach(bodies::add);

    for (BlockStmt body : bodies) {
      int calls = findLastCall(body);
      if (calls >= 0) {
        return body.getStatement(calls);
      }
    }

    return bodies.stream().flatMap(body -> body.getStatements().stream()).filter(this::isAssignment)
        .findFirst().orElse(null);
  }

  private int findLastCall(BlockStmt body) {
    if (parent.isBound()) {
      return AstModifier.findInsertionPointForBoundComponent(body);
    }

    return parent.getVariableName() == null ? -1
        : AstModifier.findInsertionPointForVariable(body, parent.getVariableName());
  }

  private boolean isAssignment(Statement statement) {
    return parent.getVariableName() != null && statement.isExpressionStmt()
        && statement.asExpressionStmt().getExpression().isAssignExpr()
        && VariableResolver.findDeclaration(statement.asExpressionStmt().getExpression()
            .asAssignExpr().getTarget()) == parent.getDeclarator();
  }

  // The new declaration sits with its sibling when the sibling is a local of the same block
  private Statement findLeadingPosition(ComponentSlice anchor, Statement attachStatement,
      boolean grouped) {
    Statement declaration = anchor.getDeclarationStatement();
    BlockStmt block = (BlockStmt) attachStatement.getParentNode().orElseThrow();
    if (declaration == null
        || declaration.getParentNode().filter(holder -> holder == block).isEmpty()) {
      return attachStatement;
    }

    int from = block.getStatements().indexOf(declaration);
    int to = block.getStatements().indexOf(attachStatement);
    if (point.isBefore()) {
      if (!grouped) {
        return declaration;
      }

      for (int i = from + 1; i < to; i++) {
        if (!anchor.getStatements().contains(block.getStatement(i))) {
          return attachStatement;
        }
      }

      return declaration;
    }

    int last = from;
    for (int i = from + 1; i < to; i++) {
      if (anchor.getStatements().contains(block.getStatement(i))) {
        last = i;
      }
    }

    return block.getStatement(last + 1);
  }

  private boolean isVarargsArgument(NodeWithArguments<?> receiver, int index) {
    if (MethodResolver.isComponentVarargs(receiver, index)) {
      return true;
    }

    Node node = (Node) receiver;
    if (receiver instanceof ObjectCreationExpr creation) {
      return parent.isCreation(creation) && MethodResolver
          .isComponentVarargs(TypeResolver.resolve(creation.getType(), creation), null, index);
    }

    MethodCallExpr call = (MethodCallExpr) receiver;
    String owner = parent.isCreation(call) ? parent.getCreationType(call) : parent.getTypeName();

    return MethodResolver.isComponentVarargs(TypeResolver.resolve(owner, node),
        call.getNameAsString(), index);
  }

  // A file that attaches every child with a call of its own keeps that style
  private boolean isOneCallPerChild(NodeWithArguments<?> receiver) {
    if (!(receiver instanceof MethodCallExpr) || parent.isCreation((Node) receiver)
        || receiver.getArguments().size() != 1) {
      return false;
    }

    return findAttachCalls().size() > 1;
  }

  private List<MethodCallExpr> findAttachCalls() {
    List<MethodCallExpr> calls = new ArrayList<>(cu.findAll(MethodCallExpr.class,
        call -> point.getMethodNames().contains(call.getNameAsString())
            && parent.isCallOnParent(call)));
    calls.sort(Comparator.comparing(call -> call.getRange().map(range -> range.begin)
        .orElse(new Position(Integer.MAX_VALUE, 0))));

    return calls;
  }

  private static boolean isUnconditional(Node node) {
    Node current = node.getParentNode().orElse(null);
    while (current != null && !(current instanceof CallableDeclaration)
        && !(current instanceof ClassOrInterfaceDeclaration)) {
      if (current instanceof IfStmt || current instanceof ForStmt || current instanceof ForEachStmt
          || current instanceof WhileStmt || current instanceof DoStmt
          || current instanceof SwitchEntry || current instanceof CatchClause
          || current instanceof LambdaExpr || current instanceof ConditionalExpr
          || (current instanceof ObjectCreationExpr creation
              && creation.getAnonymousClassBody().isPresent())) {
        return false;
      }

      current = current.getParentNode().orElse(null);
    }

    return true;
  }

  private static Statement getBodyStatement(Node node) {
    Statement statement = findBlockStatement(node);
    while (statement.getParentNode().flatMap(Node::getParentNode)
        .orElse(null) instanceof Statement) {
      statement = findBlockStatement(statement.getParentNode().orElseThrow());
    }

    return statement;
  }

  private static void insertBefore(Statement position, List<Statement> statements) {
    BlockStmt block = (BlockStmt) position.getParentNode().orElseThrow();
    int index = block.getStatements().indexOf(position);
    for (Statement statement : statements) {
      addStatement(block, index++, statement);
    }
  }

  private static void insertAfter(Statement position, List<Statement> statements) {
    BlockStmt block = (BlockStmt) position.getParentNode().orElseThrow();
    int index = block.getStatements().indexOf(position) + 1;
    for (Statement statement : statements) {
      addStatement(block, index++, statement);
    }
  }
}
