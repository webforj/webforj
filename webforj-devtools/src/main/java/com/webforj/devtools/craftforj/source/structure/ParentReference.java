package com.webforj.devtools.craftforj.source.structure;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.SuperExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.model.TargetContext;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.parser.AstModifier;
import com.webforj.devtools.craftforj.source.resolver.VariableResolver;
import java.util.ArrayList;
import java.util.List;

/**
 * The way one file talks to a parent component.
 *
 * <p>
 * A parent is reached through a variable, through {@code getBoundComponent()} in a composite,
 * through the class itself when the view extends a container, or it is created inline and has no
 * name yet. The reference tells which calls and which creation belong to that parent and writes new
 * calls the same way the file already does.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class ParentReference {

  private static final String GET_BOUND_COMPONENT = "getBoundComponent";

  private final CompilationUnit cu;
  private final SourceLocation location;
  private final List<Node> creations = new ArrayList<>();
  private VariableDeclarator declarator;
  private String variable;
  private boolean bound;
  private Expression inline;

  private ParentReference(CompilationUnit cu, SourceLocation location) {
    this.cu = cu;
    this.location = location;
  }

  /**
   * Reads how the file reaches the given parent.
   *
   * @param cu the parsed file
   * @param location the parent location
   *
   * @return the reference
   */
  static ParentReference of(CompilationUnit cu, SourceLocation location) {
    ParentReference reference = new ParentReference(cu, location);
    String name = location.getVariableName();

    if (name != null && !name.isEmpty()) {
      reference.bindVariable(ComponentSlice.findDeclarator(cu, location));

      return reference;
    }

    if (location.getLine() != null) {
      TargetContext target = new TargetContext(location.getLine(), location.getSimpleTypeName());
      reference.inline = AstFinder.findInlineCreationAt(cu, target).orElse(null);
    }

    if (reference.inline != null) {
      reference.creations.add(reference.inline);

      return reference;
    }

    VariableDeclarator alias = findBoundAlias(cu);
    if (alias != null) {
      reference.bindVariable(alias);
    } else {
      reference.bound = AstFinder.isCompositeClass(cu);
    }

    return reference;
  }

  /**
   * Gets the parent location.
   *
   * @return the parent location
   */
  SourceLocation getLocation() {
    return location;
  }

  /**
   * Gets the variable the parent is reached through.
   *
   * @return the variable name, or {@code null} when the parent has none
   */
  String getVariableName() {
    return variable;
  }

  /**
   * Checks whether the parent is the class itself, reached through unscoped calls.
   *
   * @return {@code true} when the parent has no variable and is no bound component
   */
  boolean isImplicit() {
    return variable == null && inline == null && !bound;
  }

  /**
   * Checks whether the parent is the bound component of a composite.
   *
   * @return {@code true} when calls go through {@code getBoundComponent()}
   */
  boolean isBound() {
    return bound;
  }

  /**
   * Gets the type that owns the parent's methods.
   *
   * @return the fully qualified type when known, the simple type otherwise
   */
  String getTypeName() {
    String type = location.getComponentType();

    return type == null || type.isEmpty() ? location.getSimpleTypeName() : type;
  }

  /**
   * Checks whether a call is made on the parent.
   *
   * @param call the call to check
   *
   * @return {@code true} when the call, or the fluent chain it ends, starts at the parent
   */
  boolean isCallOnParent(MethodCallExpr call) {
    Expression current = call;
    Expression next = call.getScope().orElse(null);
    while (next != null) {
      if (isCreation(next)) {
        return true;
      }

      if (next instanceof MethodCallExpr inner && isAccessor(inner)) {
        return false;
      }

      current = next;
      next = current instanceof MethodCallExpr link ? link.getScope().orElse(null) : null;
    }

    if (variable != null) {
      return VariableResolver.findDeclaration(current) == declarator;
    }

    if (inline != null) {
      return false;
    }

    if (bound) {
      return current instanceof MethodCallExpr root && isBoundComponentCall(root);
    }

    return current instanceof ThisExpr || current instanceof SuperExpr
        || (current instanceof MethodCallExpr root && root.getScope().isEmpty()
            && !isBoundComponentCall(root));
  }

  /**
   * Checks whether a node creates the parent.
   *
   * @param node a creation or a static factory call
   *
   * @return {@code true} when its arguments are handed to the new parent
   */
  boolean isCreation(Node node) {
    return creations.stream().anyMatch(creation -> creation == node);
  }

  /**
   * Gets the type a creation of the parent names.
   *
   * @param creation a node accepted by {@link #isCreation(Node)}
   *
   * @return the simple type name
   */
  String getCreationType(Node creation) {
    if (creation instanceof ObjectCreationExpr created) {
      return created.getType().getNameAsString();
    }

    return ((MethodCallExpr) creation).getScope().map(Expression::toString).orElse(null);
  }

  /**
   * Builds a call on the parent.
   *
   * <p>
   * A parent created inline is given a variable first, since a statement needs a name to call.
   * </p>
   *
   * @param method the method name
   * @param argument the argument
   *
   * @return the call, not yet placed in the file
   */
  MethodCallExpr createCall(String method, Expression argument) {
    return createCall(method, List.of(argument));
  }

  /**
   * Creates a call on the parent with several arguments, a part added through a call.
   *
   * @param method the method name
   * @param arguments the arguments
   *
   * @return the call
   */
  MethodCallExpr createCall(String method, List<Expression> arguments) {
    if (inline != null) {
      String extracted = AstModifier.extractToVariable(inline, location.getSimpleTypeName());
      if (extracted == null) {
        throw new SourceModificationException(
            "Cannot give " + ComponentSlice.describe(location) + " a variable to attach to");
      }

      SourceLocation named = new SourceLocation(location.getFile(), null,
          location.getDeclaringClass(), extracted, location.getComponentType());
      inline = null;
      creations.clear();
      bindVariable(ComponentSlice.findDeclarator(cu, named));
    }

    Expression scope = null;
    if (variable != null) {
      scope = new NameExpr(variable);
    } else if (bound) {
      scope = new MethodCallExpr(GET_BOUND_COMPONENT);
    }

    MethodCallExpr call = new MethodCallExpr(scope, method);
    arguments.forEach(call::addArgument);

    return call;
  }

  /**
   * Gets the declarator of the parent variable.
   *
   * @return the declarator, or {@code null} when the parent has no variable
   */
  VariableDeclarator getDeclarator() {
    return declarator;
  }

  private void bindVariable(VariableDeclarator found) {
    declarator = found;
    variable = declarator.getNameAsString();
    declarator.getInitializer().map(ParentReference::findCreationRoot).ifPresent(creations::add);

    for (AssignExpr assign : cu.findAll(AssignExpr.class)) {
      if (VariableResolver.findDeclaration(assign.getTarget()) == declarator) {
        Node root = findCreationRoot(assign.getValue());
        if (root != null) {
          creations.add(root);
        }
      }
    }
  }

  private static VariableDeclarator findBoundAlias(CompilationUnit cu) {
    if (!AstFinder.isCompositeClass(cu)) {
      return null;
    }

    return cu.findAll(VariableDeclarator.class).stream()
        .filter(declarator -> declarator.getInitializer().filter(
            initializer -> initializer instanceof MethodCallExpr call && isBoundComponentCall(call))
            .isPresent())
        .findFirst().orElse(null);
  }

  private static Node findCreationRoot(Expression expression) {
    Expression current = expression;
    while (current instanceof MethodCallExpr call) {
      Expression scope = call.getScope().orElse(null);
      if (scope instanceof NameExpr name
          && Character.isUpperCase(name.getNameAsString().charAt(0))) {
        return call;
      }

      current = scope;
    }

    return current instanceof ObjectCreationExpr ? current : null;
  }

  private static boolean isAccessor(MethodCallExpr call) {
    String name = call.getNameAsString();

    return call.getArguments().isEmpty() && !GET_BOUND_COMPONENT.equals(name)
        && (name.startsWith("get") || name.startsWith("is"));
  }

  private static boolean isBoundComponentCall(MethodCallExpr call) {
    return GET_BOUND_COMPONENT.equals(call.getNameAsString()) && call.getScope().isEmpty();
  }
}
