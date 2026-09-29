package com.webforj.devtools.craftforj.source.parser;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.EnclosedExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.stmt.Statement;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.resolver.VariableResolver;
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
    return AstFinder.indexOfSame(nodes, node);
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
    return VariableResolver.findDeclaration(receiver);
  }
}
