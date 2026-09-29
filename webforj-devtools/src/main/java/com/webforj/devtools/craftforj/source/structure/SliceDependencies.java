package com.webforj.devtools.craftforj.source.structure;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.PackageDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.comments.Comment;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.Name;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.SimpleName;
import com.github.javaparser.ast.expr.SuperExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.webforj.devtools.craftforj.source.SourceImports;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.resolver.TypeResolver;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * What a piece of cut source needs from the file it came from.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class SliceDependencies {

  private final CompilationUnit source;
  private final List<Node> nodes;
  private final Set<String> moved;

  SliceDependencies(CompilationUnit source, List<Node> nodes, Set<String> moved) {
    this.source = source;
    this.nodes = nodes;
    this.moved = moved;
  }

  /**
   * Refuses source that reads anything which stays behind.
   *
   * @param sameClass {@code true} when the source lands in the class it came from, where fields and
   *        methods stay reachable
   * @param visible the locals and parameters the landing place can read
   * @param fileName the file the source leaves, for the message
   *
   * @throws SourceModificationException when the source is not self contained
   */
  void requireSelfContained(boolean sameClass, Set<String> visible, String fileName) {
    Set<String> own = new LinkedHashSet<>(moved);
    own.addAll(visible);
    Set<String> fields = new LinkedHashSet<>();
    for (Node node : nodes) {
      node.findAll(Parameter.class).forEach(parameter -> own.add(parameter.getNameAsString()));
      node.findAll(VariableDeclarator.class)
          .forEach(variable -> own.add(variable.getNameAsString()));
    }

    source.findAll(ClassOrInterfaceDeclaration.class).forEach(type -> type.getFields().forEach(
        field -> field.getVariables().forEach(variable -> fields.add(variable.getNameAsString()))));

    Set<String> reachable = sameClass ? fields : Set.of();
    for (Node node : nodes) {
      requireReachableNames(node, own, reachable, fileName);
      if (!sameClass) {
        requireNoOwnCalls(node, fileName);
        requireNoSelf(node, fileName);
      }
    }
  }

  /**
   * Resolves the imports the source needs in the file it lands in.
   *
   * @param target the parsed file the source lands in
   *
   * @return the fully qualified names to import
   *
   * @throws SourceModificationException when a type cannot be resolved or collides in the target
   */
  Set<String> resolveImports(CompilationUnit target) {
    Set<String> imports = new LinkedHashSet<>();
    String sourcePackage = getPackage(source);
    String targetPackage = getPackage(target);

    for (String type : getTypeNames(nodes)) {
      String qualified = qualify(type);
      if (qualified == null && !sourcePackage.equals(targetPackage)) {
        qualified = qualifyByPackage(type, sourcePackage);
      }

      if (qualified != null) {
        requireNoOtherImport(target, type, qualified);
        String owner =
            qualified.contains(".") ? qualified.substring(0, qualified.lastIndexOf('.')) : "";
        if (!owner.equals(targetPackage) && !"java.lang".equals(owner)) {
          imports.add(qualified);
        }
      }
    }

    return imports;
  }

  /**
   * Lets the imports of the removed source follow their use in what is left of the file.
   *
   * @param imports the imports of the edit
   */
  void trackImports(SourceImports imports) {
    Set<String> left = new LinkedHashSet<>();
    source.findAll(SimpleName.class).forEach(name -> left.add(name.getIdentifier()));
    source.findAll(Name.class).stream()
        .filter(name -> AstFinder.findAncestor(name, ImportDeclaration.class).isEmpty())
        .forEach(name -> left.add(name.getIdentifier()));
    String comments =
        String.join("\n", source.getAllComments().stream().map(Comment::getContent).toList());

    Set<String> candidates = new LinkedHashSet<>();
    Set<String> used = new LinkedHashSet<>();
    for (String type : getTypeNames(nodes)) {
      for (ImportDeclaration declaration : source.getImports()) {
        String name = declaration.getNameAsString();
        if (!declaration.isAsterisk() && !declaration.isStatic() && name.endsWith("." + type)) {
          candidates.add(name);
          if (left.contains(type) || comments.matches("(?s).*\\b" + type + "\\b.*")) {
            used.add(name);
          }
        }
      }
    }

    imports.setTracked(candidates, used);
  }

  private void requireNoSelf(Node node, String fileName) {
    boolean self = node.findAll(ThisExpr.class).stream().anyMatch(
        expression -> !(expression.getParentNode().orElse(null) instanceof FieldAccessExpr access
            && moved.contains(access.getNameAsString())))
        || !node.findAll(SuperExpr.class).isEmpty();
    if (self) {
      throw new SourceModificationException(
          "The moved code reads this, which is the class in " + fileName);
    }
  }

  // A type no import names sits in the package of the file it is written in
  private String qualifyByPackage(String type, String sourcePackage) {
    boolean nested = source.findAll(TypeDeclaration.class).stream().anyMatch(
        declared -> declared.getNameAsString().equals(type) && !declared.isTopLevelType());
    if (nested) {
      throw new SourceModificationException(
          "The moved code uses " + type + ", which is declared inside the class it leaves");
    }

    if (sourcePackage.isEmpty()) {
      throw new SourceModificationException("The moved code uses " + type
          + ", which sits in the default package and cannot be imported");
    }

    return sourcePackage + "." + type;
  }

  private String qualify(String type) {
    for (ImportDeclaration declaration : source.getImports()) {
      String name = declaration.getNameAsString();
      if (!declaration.isAsterisk() && !declaration.isStatic() && name.endsWith("." + type)) {
        return name;
      }
    }

    for (ImportDeclaration declaration : source.getImports()) {
      String name = declaration.getNameAsString() + "." + type;
      if (declaration.isAsterisk() && !declaration.isStatic() && TypeResolver.load(name) != null) {
        return name;
      }
    }

    return TypeResolver.load("java.lang." + type) != null ? "java.lang." + type : null;
  }

  private static void requireReachableNames(Node node, Set<String> own, Set<String> fields,
      String fileName) {
    for (NameExpr name : node.findAll(NameExpr.class)) {
      String id = name.getNameAsString();
      boolean reachable = own.contains(id) || isTypeName(name) || fields.contains(id);
      if (!reachable) {
        throw new SourceModificationException(
            "The moved code reads " + id + ", which stays in " + fileName);
      }
    }
  }

  private static void requireNoOwnCalls(Node node, String fileName) {
    for (MethodCallExpr call : node.findAll(MethodCallExpr.class)) {
      if (call.getScope().isEmpty()) {
        throw new SourceModificationException(
            "The moved code calls " + call.getNameAsString() + "(), which stays in " + fileName);
      }
    }
  }

  private static void requireNoOtherImport(CompilationUnit target, String type, String qualified) {
    for (ImportDeclaration existing : target.getImports()) {
      String name = existing.getNameAsString();
      if (!existing.isAsterisk() && !existing.isStatic() && name.endsWith("." + type)
          && !name.equals(qualified)) {
        throw new SourceModificationException(
            "The target file already imports another " + type + ", " + name);
      }
    }
  }

  private static Set<String> getTypeNames(List<Node> nodes) {
    Set<String> types = new LinkedHashSet<>();
    for (Node node : nodes) {
      for (ClassOrInterfaceType type : node.findAll(ClassOrInterfaceType.class)) {
        ClassOrInterfaceType outer = type;
        while (outer.getScope().orElse(null) instanceof ClassOrInterfaceType scope) {
          outer = scope;
        }

        if (!"var".equals(outer.getNameAsString())) {
          types.add(outer.getNameAsString());
        }
      }

      node.findAll(NameExpr.class).stream().filter(SliceDependencies::isTypeName)
          .forEach(name -> types.add(name.getNameAsString()));
    }

    return types;
  }

  // Button in Button.create() and ButtonTheme in ButtonTheme.PRIMARY name a type, SIZE does not
  private static boolean isTypeName(NameExpr name) {
    String id = name.getNameAsString();
    Node parent = name.getParentNode().orElse(null);
    boolean scope = (parent instanceof FieldAccessExpr access && access.getScope() == name)
        || (parent instanceof MethodCallExpr call && call.getScope().orElse(null) == name);

    return scope && Character.isUpperCase(id.charAt(0)) && !id.equals(id.toUpperCase());
  }

  private static String getPackage(CompilationUnit cu) {
    return cu.getPackageDeclaration().map(PackageDeclaration::getNameAsString).orElse("");
  }
}
