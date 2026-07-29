package com.webforj.devtools.craftforj.inspector.source.parser;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.expr.TextBlockLiteralExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.expr.UnaryExpr;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.ScalarSourceGenerator;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Traces a component property to a constructor parameter of the enclosing user class and rewrites
 * the matching argument at the class's usage site.
 *
 * <p>
 * When a component is created inside a reusable class, its creation site is that class's file, and
 * editing there changes every usage. This rewriter covers the case where the property value flows
 * verbatim from a constructor parameter of the enclosing class, so the edit can instead replace the
 * literal argument at the single call site the developer is looking at.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class UsageSiteRewriter {

  /**
   * A property value traced to a constructor parameter of the enclosing class.
   *
   * @param className the simple name of the enclosing class
   * @param parameterIndex the index of the constructor parameter carrying the value
   * @param parameterCount the parameter count of the traced constructor
   * @param fromSetter whether the trace came from a setter call rather than a creation argument
   */
  public record Trace(String className, int parameterIndex, int parameterCount,
      boolean fromSetter) {}

  private UsageSiteRewriter() {}

  /**
   * Traces the property behind a setter method to constructor parameters of the enclosing class.
   *
   * <p>
   * Setter calls on the component are authoritative since the last one decides the runtime value.
   * When every such call passes a constructor parameter through unchanged, those parameters are the
   * trace. A setter call carrying anything else means the value is produced inside the class and
   * cannot be edited from a usage site. Without setter calls, every creation argument naming a
   * constructor parameter is a candidate, disambiguated later against the current value at the call
   * site.
   * </p>
   *
   * @param creationCu the parsed source of the class creating the component
   * @param target the creation line and acceptable component types
   * @param setterMethodName the setter carrying the property (e.g. "setText")
   *
   * @return the traces, or an empty list when the value does not flow from a constructor parameter
   */
  public static List<Trace> trace(CompilationUnit creationCu, TargetContext target,
      String setterMethodName) {
    ObjectCreationExpr creation = findCreationAt(creationCu, target).orElse(null);
    if (creation == null) {
      return List.of();
    }

    String componentVariable = componentVariable(creation);
    ConstructorDeclaration enclosing =
        creation.findAncestor(ConstructorDeclaration.class).orElse(null);

    // A field-initialized component has no enclosing constructor; its setter calls live in the
    // class's constructors instead
    List<ConstructorDeclaration> constructors;
    if (enclosing != null) {
      constructors = List.of(enclosing);
    } else {
      if (componentVariable == null || creation.findAncestor(FieldDeclaration.class).isEmpty()) {
        return List.of();
      }
      ClassOrInterfaceDeclaration classDecl =
          creation.findAncestor(ClassOrInterfaceDeclaration.class).orElse(null);
      if (classDecl == null) {
        return List.of();
      }
      constructors = classDecl.getConstructors();
    }

    List<Trace> traces = new ArrayList<>();
    boolean anySetter = false;
    for (ConstructorDeclaration constructor : constructors) {
      for (MethodCallExpr call : constructor.findAll(MethodCallExpr.class)) {
        if (call.getNameAsString().equals(setterMethodName) && call.getArguments().size() == 1
            && targetsComponent(call, creation, componentVariable)) {
          anySetter = true;
          Trace parameterTrace = parameterTrace(constructor, call.getArgument(0), true);
          if (parameterTrace == null) {
            return List.of();
          }
          traces.add(parameterTrace);
        }
      }
    }

    if (anySetter) {
      return traces;
    }

    if (enclosing == null) {
      return List.of();
    }

    for (Expression argument : creation.getArguments()) {
      Trace parameterTrace = parameterTrace(enclosing, argument, false);
      if (parameterTrace != null) {
        traces.add(parameterTrace);
      }
    }

    return traces;
  }

  /**
   * Replaces the traced argument of the class creation at the usage line with the new value.
   *
   * <p>
   * A literal argument must equal the property's original value. This guards against constructor
   * overloads, stale line numbers, and wrong-parameter guesses, and it disambiguates when several
   * creation arguments name constructor parameters. A setter-derived trace pins the parameter
   * exactly, so a computed argument there is replaced too and reported back, letting the caller
   * warn that the written literal erases the computation.
   * </p>
   *
   * @param usageCu the parsed source of the usage site
   * @param usageLine the line where the enclosing class is instantiated
   * @param traces the traces produced by {@link #trace}
   * @param originalValue the property value before the change
   * @param newValue the property value to write
   * @param javaType the property's Java type used to format the literal
   *
   * @return the computed expression the write overwrote as source text, or null for a literal
   */
  public static String rewrite(CompilationUnit usageCu, int usageLine, List<Trace> traces,
      Object originalValue, Object newValue, Class<?> javaType) {
    if (traces.isEmpty()) {
      throw new SourceModificationException("The property does not trace to a usage site");
    }

    String className = traces.get(0).className();
    ObjectCreationExpr usageCall = null;
    for (ObjectCreationExpr candidate : usageCu.findAll(ObjectCreationExpr.class)) {
      if (candidate.getRange().isPresent() && candidate.getRange().get().begin.line == usageLine
          && candidate.getType().getNameAsString().equals(className)) {
        usageCall = candidate;
        break;
      }
    }

    if (usageCall == null) {
      throw new SourceModificationException(
          "No " + className + " creation found at line " + usageLine);
    }

    Set<Integer> matchingIndexes = new LinkedHashSet<>();
    for (Trace candidate : traces) {
      if (usageCall.getArguments().size() != candidate.parameterCount()) {
        continue;
      }
      Expression argument = usageCall.getArgument(candidate.parameterIndex());
      if (literalEquals(argument, originalValue)) {
        matchingIndexes.add(candidate.parameterIndex());
      }
    }

    if (matchingIndexes.isEmpty()) {
      Integer definiteIndex = findDefiniteComputedIndex(usageCall, traces);
      if (definiteIndex != null) {
        String replaced = usageCall.getArgument(definiteIndex).toString();
        usageCall.setArgument(definiteIndex,
            ScalarSourceGenerator.toExpression(newValue, javaType));
        return replaced;
      }

      throw new SourceModificationException(
          "The argument at the usage site no longer matches the property value");
    }
    if (matchingIndexes.size() > 1) {
      throw new SourceModificationException(
          "Several arguments at the usage site match the property value");
    }

    usageCall.setArgument(matchingIndexes.iterator().next(),
        ScalarSourceGenerator.toExpression(newValue, javaType));
    return null;
  }

  // A stale literal is a hard stop, but a computed argument at a parameter every setter-derived
  // trace agrees on is the value's true source and is safe to replace
  private static Integer findDefiniteComputedIndex(ObjectCreationExpr usageCall,
      List<Trace> traces) {
    Set<Integer> definiteIndexes = new LinkedHashSet<>();
    for (Trace candidate : traces) {
      if (candidate.fromSetter() && usageCall.getArguments().size() == candidate.parameterCount()) {
        definiteIndexes.add(candidate.parameterIndex());
      }
    }

    if (definiteIndexes.size() != 1) {
      return null;
    }

    Integer index = definiteIndexes.iterator().next();
    Expression argument = usageCall.getArgument(index);
    boolean isPlainLiteral = argument.isLiteralExpr()
        || (argument instanceof UnaryExpr unary && unary.getOperator() == UnaryExpr.Operator.MINUS
            && unary.getExpression().isLiteralExpr());

    return isPlainLiteral ? null : index;
  }

  private static Optional<ObjectCreationExpr> findCreationAt(CompilationUnit cu,
      TargetContext target) {
    int lineNumber = target.getLineNumber();
    for (ObjectCreationExpr creation : cu.findAll(ObjectCreationExpr.class)) {
      if (creation.getRange().isPresent() && creation.getRange().get().begin.line == lineNumber
          && matchesTargetType(creation, target)) {
        return Optional.of(creation);
      }
    }

    return Optional.empty();
  }

  private static boolean matchesTargetType(ObjectCreationExpr creation, TargetContext target) {
    String typeName = creation.getType().getNameAsString();
    if (target.getAcceptableTypes() != null && !target.getAcceptableTypes().isEmpty()) {
      return target.getAcceptableTypes().contains(typeName);
    }

    return target.getTypeName() == null || target.getTypeName().isEmpty()
        || target.getTypeName().equals(typeName);
  }

  private static Trace parameterTrace(ConstructorDeclaration constructor, Expression argument,
      boolean fromSetter) {
    if (!(argument instanceof NameExpr name)) {
      return null;
    }

    List<Parameter> parameters = constructor.getParameters();
    for (int i = 0; i < parameters.size(); i++) {
      if (parameters.get(i).getNameAsString().equals(name.getNameAsString())) {
        return new Trace(constructor.getNameAsString(), i, parameters.size(), fromSetter);
      }
    }

    return null;
  }

  private static String componentVariable(ObjectCreationExpr creation) {
    Optional<VariableDeclarator> declarator = creation.findAncestor(VariableDeclarator.class);
    if (declarator.isPresent()) {
      return declarator.get().getNameAsString();
    }

    Optional<AssignExpr> assignment = creation.findAncestor(AssignExpr.class);
    if (assignment.isPresent() && assignment.get().getTarget() instanceof NameExpr name) {
      return name.getNameAsString();
    }

    return null;
  }

  private static boolean targetsComponent(MethodCallExpr call, ObjectCreationExpr creation,
      String componentVariable) {
    Expression scope = call.getScope().orElse(null);
    while (scope instanceof MethodCallExpr chained) {
      scope = chained.getScope().orElse(null);
    }

    if (scope == creation) {
      return true;
    }

    if (componentVariable == null) {
      return false;
    }

    if (scope instanceof NameExpr name) {
      return name.getNameAsString().equals(componentVariable);
    }

    // A field referenced from its own class reads as this.field
    return scope instanceof FieldAccessExpr fieldAccess
        && fieldAccess.getScope() instanceof ThisExpr
        && fieldAccess.getNameAsString().equals(componentVariable);
  }

  private static boolean literalEquals(Expression expression, Object value) {
    if (value == null) {
      return expression.isNullLiteralExpr();
    }
    if (expression instanceof StringLiteralExpr literal) {
      return literal.asString().equals(String.valueOf(value));
    }
    if (expression instanceof TextBlockLiteralExpr literal) {
      return literal.translateEscapes().equals(String.valueOf(value));
    }
    if (expression instanceof BooleanLiteralExpr literal) {
      return value instanceof Boolean bool && literal.getValue() == bool;
    }
    if (expression instanceof UnaryExpr unary && unary.getOperator() == UnaryExpr.Operator.MINUS) {
      return numericEquals(unary.getExpression(), value, -1);
    }

    return numericEquals(expression, value, 1);
  }

  private static boolean numericEquals(Expression expression, Object value, int sign) {
    if (!(expression instanceof IntegerLiteralExpr) && !(expression instanceof DoubleLiteralExpr)) {
      return false;
    }

    try {
      double literal =
          Double.parseDouble(expression.toString().replace("_", "").replaceAll("[lLfFdD]$", ""))
              * sign;
      double expected = value instanceof Number number ? number.doubleValue()
          : Double.parseDouble(String.valueOf(value));

      return literal == expected;
    } catch (NumberFormatException e) {

      return false;
    }
  }
}
