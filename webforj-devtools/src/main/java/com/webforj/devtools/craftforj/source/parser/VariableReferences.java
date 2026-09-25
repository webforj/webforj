package com.webforj.devtools.craftforj.source.parser;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.CallableDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.EnclosedExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForEachStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

/** Resolves setter receivers to their lexical variable declarations. */
final class VariableReferences {

  private VariableReferences() {}

  static boolean isCallOnVariable(MethodCallExpr call, VariableDeclarator variable) {
    return isScopeOnVariable(call, variable);
  }

  static boolean isScopeOnVariable(Expression scope, VariableDeclarator variable) {
    Expression initializer = getReceiver(variable.getInitializer().orElse(null));
    return isScopeMatching(scope,
        expression -> expression == initializer || isReferenceToVariable(expression, variable));
  }

  static boolean isScopeOnExpression(Expression scope, Expression receiver) {
    return isScopeMatching(scope, expression -> expression == receiver);
  }

  static boolean isScopeOnBoundComponent(Expression scope) {
    return isScopeMatching(scope,
        expression -> expression instanceof MethodCallExpr call
            && "getBoundComponent".equals(call.getNameAsString())
            && call.getScope().map(ThisExpr.class::isInstance).orElse(true));
  }

  static String getAccessor(Expression expression) {
    Set<Node> visited = Collections.newSetFromMap(new IdentityHashMap<>());
    Expression scope = resolveAlias(expression);
    while (scope instanceof MethodCallExpr call && visited.add(scope)) {
      if (call.getArguments().isEmpty() && call.getNameAsString().startsWith("get")) {
        return "getBoundComponent".equals(call.getNameAsString()) ? null : call.getNameAsString();
      }
      scope = resolveAlias(call.getScope().orElse(null));
    }
    return null;
  }

  static boolean isReferenceToVariable(Expression reference, VariableDeclarator variable) {
    return isReferenceToDeclaration(reference, variable);
  }

  static boolean isScopeOnDeclaration(Expression scope, Node declaration) {
    return isScopeMatching(scope, expression -> isReferenceToDeclaration(expression, declaration));
  }

  private static boolean isReferenceToDeclaration(Expression reference, Node target) {
    Set<Node> visited = Collections.newSetFromMap(new IdentityHashMap<>());
    Node declaration = resolveReceiver(reference);
    while (declaration != null && visited.add(declaration)) {
      if (declaration == target) {
        return true;
      }
      declaration = declaration instanceof VariableDeclarator candidate
          ? resolveReceiver(candidate.getInitializer().orElse(null))
          : null;
    }
    return false;
  }

  static void qualifyInsertedReceiver(Statement statement, VariableDeclarator variable) {
    qualifyReference(getReceiver(statement.asExpressionStmt().getExpression()), variable);
  }

  static void qualifyReference(Expression receiver, VariableDeclarator variable) {
    if (resolveReceiver(receiver) == variable) {
      return;
    }
    if (variable.getParentNode().orElse(null) instanceof FieldDeclaration) {
      FieldAccessExpr qualified = new FieldAccessExpr(new ThisExpr(), variable.getNameAsString());
      receiver.replace(qualified);
      if (resolveReceiver(qualified) == variable) {
        return;
      }
    }
    throw new SourceModificationException("'" + variable.getNameAsString()
        + "' is declared in another block, so the write location cannot see it");
  }

  static Expression getReceiver(Expression expression) {
    Expression current = expression;
    while (current instanceof MethodCallExpr call && call.getScope().isPresent()) {
      current = call.getScope().orElseThrow();
    }
    return current;
  }

  /**
   * Finds the position of a node in a list by identity. {@code NodeList.indexOf} compares nodes
   * structurally, so two textually identical statements in one block would report the first one.
   *
   * @param nodes the list to search
   * @param node the node instance to locate
   * @return the index of the same instance, or -1 when the list does not hold it
   */
  static int indexOfSame(List<? extends Node> nodes, Node node) {
    for (int index = 0; index < nodes.size(); index++) {
      if (nodes.get(index) == node) {
        return index;
      }
    }
    return -1;
  }

  private static boolean isScopeMatching(Expression scope, Predicate<Expression> matcher) {
    Set<Node> visited = Collections.newSetFromMap(new IdentityHashMap<>());
    Expression current = scope;
    while (current != null && visited.add(current)) {
      if (matcher.test(current)) {
        return true;
      }
      current = current instanceof MethodCallExpr call ? call.getScope().orElse(null)
          : resolveAlias(current);
    }
    return false;
  }

  private static Expression resolveAlias(Expression expression) {
    Set<Node> visited = Collections.newSetFromMap(new IdentityHashMap<>());
    Expression current = expression;
    while (true) {
      if (current instanceof EnclosedExpr enclosed) {
        current = enclosed.getInner();
      }
      Node declaration = resolveReceiver(current);
      if (!(declaration instanceof VariableDeclarator variable) || !visited.add(variable)) {
        return current;
      }
      Expression initializer = variable.getInitializer().orElse(null);
      boolean accessor = initializer instanceof MethodCallExpr call && call
          .findAll(MethodCallExpr.class).stream().anyMatch(method -> method.getArguments().isEmpty()
              && method.getNameAsString().startsWith("get"));
      if (!(initializer instanceof NameExpr || initializer instanceof FieldAccessExpr
          || initializer instanceof EnclosedExpr || accessor)) {
        return current;
      }
      current = initializer;
    }
  }

  static Node resolveReceiver(Expression receiver) {
    if (receiver instanceof EnclosedExpr enclosed) {
      return resolveReceiver(enclosed.getInner());
    }
    if (receiver instanceof NameExpr name) {
      return resolveName(name);
    }
    if (receiver instanceof FieldAccessExpr field && field.getScope() instanceof ThisExpr self) {
      String qualifier = self.getTypeName().map(Node::toString).orElse(null);
      Node scope = field.getParentNode().orElse(null);
      while (scope != null) {
        if (isClassScope(scope) && (qualifier == null || scope instanceof TypeDeclaration<?> type
            && type.getNameAsString().equals(qualifier))) {
          return findField(scope, field.getNameAsString());
        }
        scope = scope.getParentNode().orElse(null);
      }
    }
    return null;
  }

  private static Node resolveName(NameExpr reference) {
    String name = reference.getNameAsString();
    Node child = reference;
    Node scope = child.getParentNode().orElse(null);
    while (scope != null) {
      Node declaration = findVisibleDeclaration(scope, child, name);
      if (declaration != null) {
        return declaration;
      }
      child = scope;
      scope = scope.getParentNode().orElse(null);
    }
    return null;
  }

  private static Node findVisibleDeclaration(Node scope, Node child, String name) {
    if (scope instanceof BlockStmt block) {
      return findInStatements(block.getStatements(), indexOfSame(block.getStatements(), child),
          name);
    }

    if (scope instanceof VariableDeclarationExpr variables) {
      return findVariable(variables, name, indexOfSame(variables.getVariables(), child) + 1);
    }

    if (scope instanceof CallableDeclaration<?> callable) {
      return findParameter(callable.getParameters(), name);
    }

    if (scope instanceof LambdaExpr function) {
      return findParameter(function.getParameters(), name);
    }

    if (scope instanceof CatchClause clause) {
      return findParameter(List.of(clause.getParameter()), name);
    }

    if (scope instanceof ForEachStmt loop && child == loop.getBody()) {
      return findVariable(loop.getVariable(), name, loop.getVariable().getVariables().size());
    }

    if (scope instanceof ForStmt loop) {
      return findInInitialization(loop.getInitialization(),
          indexOfSame(loop.getInitialization(), child), name);
    }

    return isClassScope(scope) ? findField(scope, name) : null;
  }

  private static Node findInStatements(List<Statement> statements, int before, String name) {
    for (int index = before - 1; index >= 0; index--) {
      if (statements.get(index) instanceof ExpressionStmt statement
          && statement.getExpression() instanceof VariableDeclarationExpr variables) {
        Node match = findVariable(variables, name, variables.getVariables().size());
        if (match != null) {
          return match;
        }
      }
    }

    return null;
  }

  private static Node findInInitialization(List<Expression> initialization, int before,
      String name) {
    int limit = before < 0 ? initialization.size() : before;
    for (int index = limit - 1; index >= 0; index--) {
      if (initialization.get(index) instanceof VariableDeclarationExpr variables) {
        Node match = findVariable(variables, name, variables.getVariables().size());
        if (match != null) {
          return match;
        }
      }
    }

    return null;
  }

  private static Node findParameter(List<Parameter> parameters, String name) {
    return parameters.stream().filter(parameter -> parameter.getNameAsString().equals(name))
        .findFirst().orElse(null);
  }

  private static VariableDeclarator findVariable(VariableDeclarationExpr variables, String name,
      int limit) {
    for (int index = limit - 1; index >= 0; index--) {
      VariableDeclarator variable = variables.getVariable(index);
      if (variable.getNameAsString().equals(name)) {
        return variable;
      }
    }
    return null;
  }

  private static boolean isClassScope(Node node) {
    return node instanceof TypeDeclaration<?> || node instanceof ObjectCreationExpr creation
        && creation.getAnonymousClassBody().isPresent();
  }

  private static VariableDeclarator findField(Node scope, String name) {
    return scope.getChildNodes().stream().filter(FieldDeclaration.class::isInstance)
        .map(FieldDeclaration.class::cast).flatMap(field -> field.getVariables().stream())
        .filter(variable -> variable.getNameAsString().equals(name)).findFirst().orElse(null);
  }
}
