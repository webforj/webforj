package com.webforj.devtools.craftforj.source.parser;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.type.TypeParameter;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import java.util.Collection;
import java.util.Set;

/** Binds generated type references without changing existing application name bindings. */
public final class TypeReferences {

  private TypeReferences() {}

  /**
   * Qualifies generated types when their simple names cannot safely be imported.
   *
   * @param cu the application source, before these generated expressions are inserted
   * @param expressions generated expressions whose imports describe their type references
   * @param imports the fully qualified types referenced by those expressions
   * @param requiredImports the shared imports reserved by this source edit
   */
  public static void bind(CompilationUnit cu, Collection<Expression> expressions,
      Collection<String> imports, Set<String> requiredImports) {
    for (String qualifiedName : imports) {
      String simpleName = qualifiedName.substring(qualifiedName.lastIndexOf('.') + 1);
      String reference = resolveType(cu, qualifiedName, requiredImports, false);
      if (!reference.equals(simpleName)) {
        for (Expression expression : expressions) {
          qualify(cu, expression, simpleName, reference);
        }
      }
    }
  }

  /**
   * Resolves a generated declaration or expression type without taking an application's name.
   *
   * @param cu the application source
   * @param qualifiedName the fully qualified type to reference
   * @param requiredImports the imports reserved by this source edit
   * @param existingType whether runtime identity establishes this type for existing references
   * @return a safe qualified name, or a simple name with its import reserved
   */
  public static String resolveType(CompilationUnit cu, String qualifiedName,
      Set<String> requiredImports, boolean existingType) {
    String simpleName = qualifiedName.substring(qualifiedName.lastIndexOf('.') + 1);
    if (requiresQualification(cu, qualifiedName, simpleName, requiredImports, existingType)) {
      requirePackageName(cu, qualifiedName, false);
      return qualifiedName;
    }
    requiredImports.add(qualifiedName);
    return simpleName;
  }

  private static boolean requiresQualification(CompilationUnit cu, String qualifiedName,
      String simpleName, Set<String> requiredImports, boolean existingType) {
    if (hasDeclaration(cu, simpleName, true) || requiredImports.stream()
        .anyMatch(name -> !name.equals(qualifiedName) && name.endsWith("." + simpleName))) {
      return true;
    }

    if (cu.getImports().stream().anyMatch(
        imported -> !imported.isAsterisk() && !imported.getNameAsString().equals(qualifiedName)
            && imported.getName().getIdentifier().equals(simpleName))) {
      return true;
    }

    boolean alreadyBound = requiredImports.contains(qualifiedName)
        || cu.getImports().stream().anyMatch(imported -> !imported.isAsterisk()
            && !imported.isStatic() && imported.getNameAsString().equals(qualifiedName));
    if (alreadyBound) {
      return false;
    }

    return cu.getImports().stream().anyMatch(imported -> imported.isAsterisk())
        || !existingType && (cu.findAll(NameExpr.class).stream()
            .anyMatch(name -> name.getNameAsString().equals(simpleName))
            || cu.findAll(ClassOrInterfaceType.class).stream().anyMatch(
                type -> type.getScope().isEmpty() && type.getNameAsString().equals(simpleName)));
  }

  private static boolean hasDeclaration(CompilationUnit cu, String name, boolean includeValues) {
    return cu.findAll(TypeDeclaration.class).stream()
        .anyMatch(type -> type.getNameAsString().equals(name))
        || cu.findAll(TypeParameter.class).stream()
            .anyMatch(type -> type.getNameAsString().equals(name))
        || includeValues && (cu.findAll(VariableDeclarator.class).stream()
            .anyMatch(variable -> variable.getNameAsString().equals(name))
            || cu.findAll(Parameter.class).stream()
                .anyMatch(parameter -> parameter.getNameAsString().equals(name)));
  }

  private static void requirePackageName(CompilationUnit cu, String qualifiedName,
      boolean expressionScope) {
    String root = qualifiedName.substring(0, qualifiedName.indexOf('.'));
    if (hasDeclaration(cu, root, expressionScope)) {
      throw new SourceModificationException("Cannot reference '" + qualifiedName
          + "': package name '" + root + "' is shadowed by an application declaration");
    }
  }

  private static void qualify(CompilationUnit cu, Expression expression, String simpleName,
      String qualifiedName) {
    for (NameExpr name : expression.findAll(NameExpr.class)) {
      if (!name.getNameAsString().equals(simpleName)) {
        continue;
      }
      boolean typeScope = name.getParentNode().map(
          parent -> parent instanceof MethodCallExpr call && call.getScope().orElse(null) == name
              || parent instanceof FieldAccessExpr field && field.getScope() == name)
          .orElse(false);
      if (typeScope) {
        requirePackageName(cu, qualifiedName, true);
        name.replace(StaticJavaParser.parseExpression(qualifiedName));
      }
    }
    for (ClassOrInterfaceType type : expression.findAll(ClassOrInterfaceType.class)) {
      if (type.getScope().isEmpty() && type.getNameAsString().equals(simpleName)) {
        type.setScope(StaticJavaParser
            .parseClassOrInterfaceType(qualifiedName.substring(0, qualifiedName.lastIndexOf('.'))));
      }
    }
  }
}
