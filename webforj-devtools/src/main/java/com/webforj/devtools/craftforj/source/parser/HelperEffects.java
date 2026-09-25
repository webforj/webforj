package com.webforj.devtools.craftforj.source.parser;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.type.Type;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceChange;
import com.webforj.devtools.craftforj.source.resolver.SourceFileResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;

/** Locates synchronous helper writes that run after the selected component's own initialization. */
final class HelperEffects {

  private final Set<MethodDeclaration> path = Collections.newSetFromMap(new IdentityHashMap<>());

  private HelperEffects() {}

  /**
   * Finds the last statement of a block that hands the component to a method writing the property.
   *
   * <p>
   * A setter placed after that statement is the final synchronous write, so the caller can keep its
   * own write authoritative without editing the helper.
   * </p>
   *
   * @param block the creation scope
   * @param receiver matches expressions that denote the selected component
   * @param change the property write
   * @return the statement whose helper writes the property, or null when none does
   * @throws SourceModificationException when a method receiving the component cannot be resolved
   */
  static Statement findLastWrite(BlockStmt block, Predicate<Expression> receiver,
      SourceChange change) {
    HelperEffects effects = new HelperEffects();
    Statement last = null;
    int lastIndex = -1;
    for (MethodCallExpr invocation : block.findAll(MethodCallExpr.class)) {
      if (!AstModifier.isInExecutionScope(invocation, block)
          || !effects.writesProperty(invocation, block, receiver, expression -> false, change)) {
        continue;
      }
      Statement statement = enclosingStatement(invocation, block);
      int index = VariableReferences.indexOfSame(block.getStatements(), statement);
      if (index > lastIndex) {
        last = statement;
        lastIndex = index;
      }
    }

    return last;
  }

  private boolean writesProperty(MethodCallExpr invocation, BlockStmt block,
      Predicate<Expression> receiver, Predicate<Expression> accessorReceiver, SourceChange change) {
    Node owner = block.getParentNode().orElse(null);
    while (owner != null && !(owner instanceof TypeDeclaration<?>)
        && !(owner instanceof ObjectCreationExpr creation
            && creation.getAnonymousClassBody().isPresent())) {
      owner = owner.getParentNode().orElse(null);
    }
    if (owner == null) {
      return false;
    }
    List<MethodDeclaration> methods = findMethods(invocation, owner);
    if (methods == null) {
      if (invocation.getArguments().stream()
          .anyMatch(argument -> receiver.test(argument) || accessorReceiver.test(argument))) {
        throw new SourceModificationException(change.getPropertyName()
            + " cannot be written: the source of '" + invocation.getNameAsString()
            + "', which receives this component, is not available");
      }
      return false;
    }
    for (MethodDeclaration method : methods) {
      if (!method.getNameAsString().equals(invocation.getNameAsString())
          || method.getParameters().size() != invocation.getArguments().size()
          || method.getBody().isEmpty() || !path.add(method)) {
        continue;
      }
      List<Node> parameters = new ArrayList<>();
      List<Node> accessorParameters = new ArrayList<>();
      for (int index = 0; index < invocation.getArguments().size(); index++) {
        Expression argument = invocation.getArgument(index);
        if (accessorReceiver.test(argument)
            || receiver.test(argument) && change.getAccessor() != null
                && change.getAccessor().equals(VariableReferences.getAccessor(argument))) {
          accessorParameters.add(method.getParameter(index));
        } else if (receiver.test(argument)) {
          parameters.add(method.getParameter(index));
        }
      }
      Predicate<Expression> helperReceiver =
          expression -> receiver.test(expression) || parameters.stream().anyMatch(
              parameter -> VariableReferences.isScopeOnDeclaration(expression, parameter));
      Predicate<Expression> helperAccessor =
          expression -> accessorReceiver.test(expression) || accessorParameters.stream().anyMatch(
              parameter -> VariableReferences.isScopeOnDeclaration(expression, parameter));
      BlockStmt body = method.getBody().orElseThrow();
      boolean writes = false;
      for (MethodCallExpr call : body.findAll(MethodCallExpr.class)) {
        if (!AstModifier.isInExecutionScope(call, body)) {
          continue;
        }
        if ((helperReceiver.test(call) || helperAccessor.test(call))
            && isPropertyWrite(call, change, helperAccessor.test(call))
            || writesProperty(call, body, helperReceiver, helperAccessor, change)) {
          writes = true;
          break;
        }
      }
      path.remove(method);
      if (writes) {
        return true;
      }
    }

    return false;
  }

  private static Statement enclosingStatement(Node node, BlockStmt block) {
    Node current = node;
    while (current.getParentNode().orElse(null) != block) {
      current = current.getParentNode().orElseThrow();
    }

    return (Statement) current;
  }

  private List<MethodDeclaration> findMethods(MethodCallExpr invocation, Node owner) {
    CompilationUnit cu = invocation.findCompilationUnit().orElseThrow();
    Node target = owner;
    if (!isOwnerCall(invocation, owner)) {
      Expression scope = invocation.getScope().orElseThrow();
      if (VariableReferences.isScopeOnBoundComponent(scope)) {
        return List.of();
      }
      String type = declaredTypeName(VariableReferences.resolveReceiver(scope), scope);
      if (type == null) {
        return null;
      }
      int generic = type.indexOf('<');
      if (generic >= 0) {
        type = type.substring(0, generic);
      }
      final String simpleType = type.substring(type.lastIndexOf('.') + 1);
      target = cu.findAll(TypeDeclaration.class).stream()
          .filter(candidate -> candidate.getNameAsString().equals(simpleType)).findFirst()
          .orElse(null);
      if (target == null) {
        final String requestedType = type;
        String imported = cu.getImports().stream()
            .filter(candidate -> !candidate.isStatic() && !candidate.isAsterisk()
                && candidate.getName().getIdentifier().equals(requestedType))
            .map(candidate -> candidate.getNameAsString()).findFirst().orElse(null);
        String qualified = imported != null ? imported : type;
        if (qualified.startsWith("com.webforj.") || qualified.startsWith("java.")
            || qualified.startsWith("System.")) {
          return List.of();
        }
        if (!qualified.contains(".")) {
          qualified = cu.getPackageDeclaration().map(pkg -> pkg.getNameAsString() + ".").orElse("")
              + qualified;
        }
        String file = SourceFileResolver.resolve(qualified, SourceFileResolver.JAVA_ONLY);
        if (file == null) {
          // An explicitly imported type without project source is a library type, and a library
          // cannot hold one of the application's helpers. A same-package type without source stays
          // unresolved.
          return imported != null ? List.of() : null;
        }
        CompilationUnit external;
        try {
          external = SourceParserService.getCurrent().parse(Path.of(file)).orElse(null);
        } catch (IOException | RuntimeException e) {
          return null;
        }
        if (external == null) {
          return null;
        }
        target = external.findAll(TypeDeclaration.class).stream()
            .filter(candidate -> candidate.getNameAsString().equals(simpleType)).findFirst()
            .orElse(null);
      }
    }
    return target == null ? null
        : target.getChildNodes().stream().filter(MethodDeclaration.class::isInstance)
            .map(MethodDeclaration.class::cast)
            .filter(method -> method.getNameAsString().equals(invocation.getNameAsString()))
            .toList();
  }

  private static String declaredTypeName(Node declaration, Expression scope) {
    Type declared = declaration instanceof VariableDeclarator variable ? variable.getType()
        : declaration instanceof Parameter parameter ? parameter.getType() : null;
    if (declared == null) {
      return scope.toString();
    }
    if (!declared.isVarType()) {
      return declared.asString();
    }
    Expression initializer =
        declaration instanceof VariableDeclarator variable ? variable.getInitializer().orElse(null)
            : null;
    return initializer instanceof ObjectCreationExpr creation ? creation.getType().getNameAsString()
        : null;
  }

  private static boolean isOwnerCall(MethodCallExpr call, Node owner) {
    Expression scope = call.getScope().orElse(null);
    if (scope == null || scope.isThisExpr()) {
      return true;
    }

    return owner instanceof TypeDeclaration<?> type && scope.isNameExpr()
        && scope.asNameExpr().getNameAsString().equals(type.getNameAsString());
  }

  private static boolean isPropertyWrite(MethodCallExpr call, SourceChange change,
      boolean accessorTarget) {
    boolean methodMatches =
        call.getNameAsString().equals(change.getMethodName()) || change.getMethodExpansions()
            .getOrDefault(call.getNameAsString(), List.of()).contains(change.getMethodName());
    if (!methodMatches || !Objects.equals(VariableReferences.getAccessor(call),
        accessorTarget ? null : change.getAccessor())) {
      return false;
    }
    if (change.getMatchKey() == null) {
      return true;
    }
    return !call.getArguments().isEmpty() && (!call.getArgument(0).isStringLiteralExpr()
        || call.getArgument(0).asStringLiteralExpr().asString().equals(change.getMatchKey()));
  }
}
