package com.webforj.devtools.craftforj.source.site;

import com.github.javaparser.Position;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.BodyDeclaration;
import com.github.javaparser.ast.body.CallableDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.EnumConstantDeclaration;
import com.github.javaparser.ast.body.EnumDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.InitializerDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.nodeTypes.NodeWithTypeParameters;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.type.ArrayType;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.type.Type;
import com.github.javaparser.ast.type.UnknownType;
import com.github.javaparser.ast.type.VarType;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.site.model.CreationSite;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Finds the expression of a source file a creation site stands for.
 *
 * <p>
 * The expression is looked up by its place among the expressions of the same kind and name inside
 * its method, never by its line. A method that holds another number of those expressions than the
 * running application was changed in a way that leaves no safe answer and is refused.
 * </p>
 *
 * <p>
 * A lambda, an anonymous class and a local class have no name the source writes. They are found by
 * what they hold, and refused when more than one of them fits.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class SourceSites {

  private static final String CONSTRUCTOR = "<init>";
  private static final String STATIC_INITIALIZER = "<clinit>";
  private static final String LAMBDA = "lambda$";

  private SourceSites() {}

  /**
   * Finds the expression a site stands for.
   *
   * @param cu the parsed file that declares the class of the site
   * @param site the site
   *
   * @return the creation or the call
   *
   * @throws SourceModificationException when the file does not hold the expression the running
   *         application created the component with
   */
  public static Expression find(CompilationUnit cu, CreationSite site) {
    if (site.getKind() == CreationSite.Kind.DELEGATION) {
      throw new SourceModificationException(
          "A constructor that hands over to another one creates no component by itself");
    }

    if (site.getKind() == CreationSite.Kind.BRIDGE) {
      throw new SourceModificationException(
          "A method the compiler wrote is not a part of the source");
    }

    List<Scope> scopes = findScopes(cu, site);
    List<Expression> found = new ArrayList<>();
    for (Scope scope : scopes) {
      for (List<Node> bodies : findBodies(scope, site)) {
        List<Expression> expressions = new ArrayList<>();
        bodies.forEach(body -> expressions.addAll(collect(body, site)));
        if (expressions.size() == site.getCount() && site.getOrdinal() < expressions.size()) {
          found.add(expressions.get(site.getOrdinal()));
        }
      }
    }

    if (found.isEmpty()) {
      throw new SourceModificationException(describe(scopes.get(0), site)
          + " was changed after the application was compiled, save and reload it first");
    }

    // The line is asked only where the code alone leaves more than one answer
    List<Expression> held = found.size() == 1 ? found
        : found.stream().filter(expression -> isAtLine(expression, site)).toList();
    if (held.size() != 1) {
      throw new SourceModificationException(scopes.get(0).getRootName() + " holds " + found.size()
          + " places that fit the code that created this component, which one cannot be told");
    }

    return held.get(0);
  }

  private static boolean isAtLine(Expression expression, CreationSite site) {
    Node holder = site.getMethodName().startsWith(LAMBDA)
        ? AstFinder.findAncestor(expression, LambdaExpr.class).orElse(null)
        : Scope.findOwner(expression);

    return holder != null && holder.getRange()
        .map(range -> range.begin.line <= site.getLine() && site.getLine() <= range.end.line)
        .orElse(false);
  }

  // A class the compiler numbered stands for every class of that kind its owner holds, the one
  // that holds the expression is told by what it holds
  private static List<Scope> findScopes(CompilationUnit cu, CreationSite site) {
    String name = site.getClassName().substring(site.getClassName().lastIndexOf('.') + 1);
    String[] names = name.split("\\$");
    List<Scope> scopes = new ArrayList<>();
    cu.getTypes().stream().filter(candidate -> candidate.getNameAsString().equals(names[0]))
        .findFirst().ifPresent(type -> scopes.add(new Scope(type, names[0])));
    boolean numbered = false;
    for (int index = 1; index < names.length; index++) {
      String nested = names[index];
      numbered |= !nested.isEmpty() && Character.isDigit(nested.charAt(0));
      List<Scope> holders = List.copyOf(scopes);
      scopes.clear();
      holders.forEach(holder -> scopes.addAll(holder.findNested(nested)));
    }

    if (scopes.isEmpty()) {
      throw new SourceModificationException(
          numbered ? names[0] + " no longer holds the class that created this component"
              : "The source no longer declares " + name.replace('$', '.'));
    }

    return scopes;
  }

  private static List<List<Node>> findBodies(Scope scope, CreationSite site) {
    String method = site.getMethodName();
    if (method.startsWith(LAMBDA)) {
      return findLambdaBodies(scope, site);
    }

    if (STATIC_INITIALIZER.equals(method)) {
      return List.of(findInitializers(scope, true));
    }

    if (!CONSTRUCTOR.equals(method)) {
      List<CallableDeclaration<?>> methods = new ArrayList<>();
      scope.getMembers().stream().filter(MethodDeclaration.class::isInstance)
          .map(MethodDeclaration.class::cast)
          .filter(candidate -> candidate.getNameAsString().equals(method)).forEach(methods::add);
      CallableDeclaration<?> callable = pick(site, methods, false);

      return callable == null ? List.of()
          : List.of(new ArrayList<>(callable.findAll(BlockStmt.class,
              block -> block.getParentNode().orElse(null) == callable)));
    }

    List<CallableDeclaration<?>> constructors = new ArrayList<>();
    scope.getMembers().stream().filter(ConstructorDeclaration.class::isInstance)
        .map(ConstructorDeclaration.class::cast).forEach(constructors::add);
    if (constructors.isEmpty()) {
      return List.of(findInitializers(scope, false));
    }

    ConstructorDeclaration constructor = (ConstructorDeclaration) pick(site, constructors, true);
    if (constructor == null) {
      return List.of();
    }

    List<Statement> statements = new ArrayList<>(constructor.getBody().getStatements());
    List<Node> bodies = new ArrayList<>();
    boolean delegates = false;
    if (!statements.isEmpty()
        && statements.get(0) instanceof ExplicitConstructorInvocationStmt invocation) {
      bodies.add(statements.remove(0));
      delegates = invocation.isThis();
    }

    // The compiler runs the initializers of the class right after the call to super
    if (!delegates) {
      bodies.addAll(findInitializers(scope, false));
    }

    bodies.addAll(statements);

    return List.of(bodies);
  }

  // The compiler turns every lambda into a method of the class and numbers it, the number follows
  // no order the source shows
  private static List<List<Node>> findLambdaBodies(Scope scope, CreationSite site) {
    List<String> compiled = readParameters(site.getDescriptor());
    String[] parts = site.getMethodName().split("\\$");
    String holder = parts.length > 2 ? parts[1] : null;
    List<List<Node>> bodies = new ArrayList<>();
    for (LambdaExpr lambda : scope.findLambdas()) {
      boolean fits = lambda.getParameters().size() <= compiled.size()
          && hasParameters(lambda.getParameters(), findTypeVariables(lambda), compiled);
      if (fits && isHeldBy(lambda, holder)) {
        bodies.add(List.of(lambda.getBody()));
      }
    }

    return bodies;
  }

  // The name of the method of a lambda carries the name of the member the lambda is written in
  private static boolean isHeldBy(LambdaExpr lambda, String holder) {
    if (holder == null) {
      return true;
    }

    Node member = AstFinder.findAncestor(lambda, BodyDeclaration.class).orElse(null);
    if (member instanceof MethodDeclaration method) {
      return method.getNameAsString().equals(holder);
    }

    if (member instanceof ConstructorDeclaration) {
      return "new".equals(holder);
    }

    if (member instanceof FieldDeclaration field) {
      return (field.isStatic() ? "static" : "new").equals(holder);
    }

    return member instanceof InitializerDeclaration block
        && (block.isStatic() ? "static" : "new").equals(holder);
  }

  private static List<Node> findInitializers(Scope scope, boolean statics) {
    List<Node> bodies = new ArrayList<>();
    if (statics) {
      bodies.addAll(scope.findConstantArguments());
    }

    for (BodyDeclaration<?> member : scope.getMembers()) {
      if (member instanceof FieldDeclaration field && field.isStatic() == statics) {
        field.getVariables().stream().map(VariableDeclarator::getInitializer)
            .forEach(initializer -> initializer.ifPresent(bodies::add));
      } else if (member instanceof InitializerDeclaration block && block.isStatic() == statics) {
        bodies.add(block.getBody());
      }
    }

    return bodies;
  }

  private static CallableDeclaration<?> pick(CreationSite site,
      List<CallableDeclaration<?>> candidates, boolean constructor) {
    List<String> compiled = readParameters(site.getDescriptor());
    List<CallableDeclaration<?>> sized = candidates.stream()
        .filter(candidate -> constructor ? candidate.getParameters().size() <= compiled.size()
            : candidate.getParameters().size() == compiled.size())
        .toList();
    List<CallableDeclaration<?>> typed =
        sized.stream().filter(candidate -> hasParameters(candidate.getParameters(),
            findTypeVariables(candidate), compiled)).toList();
    List<CallableDeclaration<?>> matches = typed.isEmpty() && sized.size() == 1 ? sized : typed;

    return matches.size() == 1 ? matches.get(0) : null;
  }

  // The constructor of an inner class or of an enum and the method of a lambda take parameters
  // the source does not write, they come first, so the written parameters are compared with the
  // last compiled ones. A parameter written without a type fits any.
  private static boolean hasParameters(List<Parameter> written, Set<String> variables,
      List<String> compiled) {
    int shift = compiled.size() - written.size();
    for (int index = 0; index < written.size(); index++) {
      Type type = written.get(index).getType();
      String name = getName(written.get(index));
      boolean typed = !(type instanceof UnknownType || type instanceof VarType);
      if (typed && !variables.contains(name) && !name.equals(compiled.get(index + shift))) {
        return false;
      }
    }

    return true;
  }

  private static Set<String> findTypeVariables(Node node) {
    Set<String> variables = new HashSet<>();
    Node owner = node;
    while (owner != null) {
      if (owner instanceof NodeWithTypeParameters<?> declared) {
        declared.getTypeParameters().forEach(variable -> variables.add(variable.getNameAsString()));
      }

      owner = owner.getParentNode().orElse(null);
    }

    List.copyOf(variables).forEach(variable -> variables.add(variable + "[]"));

    return variables;
  }

  private static String getName(Parameter parameter) {
    Type type = parameter.getType();
    StringBuilder suffix = new StringBuilder(parameter.isVarArgs() ? "[]" : "");
    while (type instanceof ArrayType array) {
      suffix.append("[]");
      type = array.getComponentType();
    }

    String name =
        type instanceof ClassOrInterfaceType named ? named.getNameAsString() : type.asString();

    return name + suffix;
  }

  private static List<String> readParameters(String descriptor) {
    List<String> parameters = new ArrayList<>();
    int index = 1;
    while (descriptor.charAt(index) != ')') {
      StringBuilder suffix = new StringBuilder();
      while (descriptor.charAt(index) == '[') {
        suffix.append("[]");
        index++;
      }

      char kind = descriptor.charAt(index);
      if (kind == 'L') {
        int end = descriptor.indexOf(';', index);
        String name = descriptor.substring(index + 1, end);
        name = name.substring(name.lastIndexOf('/') + 1);
        parameters.add(name.substring(name.lastIndexOf('$') + 1) + suffix);
        index = end + 1;
      } else {
        parameters.add(getPrimitive(kind) + suffix);
        index++;
      }
    }

    return parameters;
  }

  private static String getPrimitive(char kind) {
    return switch (kind) {
      case 'Z' -> "boolean";
      case 'B' -> "byte";
      case 'C' -> "char";
      case 'S' -> "short";
      case 'I' -> "int";
      case 'J' -> "long";
      case 'F' -> "float";
      default -> "double";
    };
  }

  private static List<Expression> collect(Node body, CreationSite site) {
    List<Expression> expressions = new ArrayList<>();
    body.walk(Expression.class, expression -> {
      if (isSite(expression, site) && isRunBy(body, expression)) {
        expressions.add(expression);
      }
    });

    // A call runs once its receiver and its arguments have run, which is the order the
    // expressions end in
    expressions.sort(Comparator.comparing((Expression expression) -> getRunPosition(expression))
        .thenComparing(SourceSites::getEnd));

    return expressions;
  }

  // The update of a loop is written before the body and runs after it
  private static Position getRunPosition(Expression expression) {
    Node child = expression;
    Node current = expression.getParentNode().orElse(null);
    while (current != null && !(current instanceof BodyDeclaration<?>)) {
      if (current instanceof ForStmt loop && AstFinder.indexOfSame(loop.getUpdate(), child) >= 0) {
        return getEnd(loop.getBody());
      }

      child = current;
      current = current.getParentNode().orElse(null);
    }

    return getEnd(expression);
  }

  private static Position getEnd(Node node) {
    return node.getEnd().orElse(new Position(Integer.MAX_VALUE, 0));
  }

  private static boolean isSite(Expression expression, CreationSite site) {
    if (site.getKind() == CreationSite.Kind.CALL) {
      return expression instanceof MethodCallExpr call
          && call.getNameAsString().equals(site.getName());
    }

    if (!(expression instanceof ObjectCreationExpr creation)) {
      return false;
    }

    String name =
        creation.getAnonymousClassBody().isPresent() ? "" : creation.getType().getNameAsString();

    return name.equals(site.getName());
  }

  // The body of a lambda and the members of a class written inside the method are compiled into
  // methods of their own
  private static boolean isRunBy(Node body, Expression expression) {
    Node current = expression == body ? body : expression.getParentNode().orElse(null);
    while (current != null && current != body) {
      if (current instanceof LambdaExpr || current instanceof BodyDeclaration<?>) {
        return false;
      }

      current = current.getParentNode().orElse(null);
    }

    return true;
  }

  private static String describe(Scope scope, CreationSite site) {
    String method = site.getMethodName();
    if (method.startsWith(LAMBDA)) {
      return "A lambda of " + scope.getRootName();
    }

    if (CONSTRUCTOR.equals(method)) {
      return "The constructor of " + scope.getName();
    }

    if (STATIC_INITIALIZER.equals(method)) {
      return "The static initializer of " + scope.getName();
    }

    return scope.getName() + "." + method + "()";
  }

  /**
   * A class of the source, declared by name or written as the body of a creation.
   */
  private static final class Scope {

    private final Node node;
    private final String rootName;

    Scope(Node node, String rootName) {
      this.node = node;
      this.rootName = rootName;
    }

    String getRootName() {
      return rootName;
    }

    String getName() {
      return node instanceof ObjectCreationExpr creation ? creation.getType().getNameAsString()
          : ((TypeDeclaration<?>) node).getNameAsString();
    }

    List<BodyDeclaration<?>> getMembers() {
      return node instanceof ObjectCreationExpr creation
          ? new ArrayList<>(creation.getAnonymousClassBody().orElseThrow())
          : new ArrayList<>(((TypeDeclaration<?>) node).getMembers());
    }

    // The constants of an enum are created by its static initializer, before anything else
    List<Node> findConstantArguments() {
      List<Node> arguments = new ArrayList<>();
      if (node instanceof EnumDeclaration values) {
        values.getEntries().forEach(entry -> arguments.addAll(entry.getArguments()));
      }

      return arguments;
    }

    List<LambdaExpr> findLambdas() {
      return node.findAll(LambdaExpr.class, lambda -> findOwner(lambda) == node);
    }

    // The compiler names a member class by its name, an anonymous class by a number and a local
    // class by a number followed by its name
    List<Scope> findNested(String name) {
      int start = 0;
      while (start < name.length() && Character.isDigit(name.charAt(start))) {
        start++;
      }

      String written = name.substring(start);
      List<Scope> nested = new ArrayList<>();
      if (written.isEmpty() && start > 0) {
        node.findAll(ObjectCreationExpr.class,
            creation -> creation.getAnonymousClassBody().isPresent() && findOwner(creation) == node)
            .forEach(creation -> nested.add(new Scope(creation, rootName)));
      } else if (!written.isEmpty()) {
        boolean member = start == 0;
        for (TypeDeclaration<?> type : node.findAll(TypeDeclaration.class)) {
          boolean direct = type.getParentNode().orElse(null) == node;
          if (type.getNameAsString().equals(written) && findOwner(type) == node
              && member == direct) {
            nested.add(new Scope(type, rootName));
          }
        }
      }

      return nested;
    }

    // What is written in the arguments of a creation belongs to the class around the creation,
    // only the members of its body belong to the class the creation declares
    static Node findOwner(Node node) {
      Node child = node;
      Node current = node.getParentNode().orElse(null);
      while (current != null) {
        boolean body = child instanceof BodyDeclaration<?> && (current instanceof ObjectCreationExpr
            || current instanceof EnumConstantDeclaration);
        if (body || current instanceof TypeDeclaration<?>) {
          return current;
        }

        child = current;
        current = current.getParentNode().orElse(null);
      }

      return null;
    }
  }
}
