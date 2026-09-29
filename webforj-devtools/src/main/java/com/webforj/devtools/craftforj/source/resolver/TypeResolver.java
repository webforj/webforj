package com.webforj.devtools.craftforj.source.resolver;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.BinaryExpr;
import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.CastExpr;
import com.github.javaparser.ast.expr.CharLiteralExpr;
import com.github.javaparser.ast.expr.ClassExpr;
import com.github.javaparser.ast.expr.ConditionalExpr;
import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.EnclosedExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.InstanceOfExpr;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.LongLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.expr.SuperExpr;
import com.github.javaparser.ast.expr.TextBlockLiteralExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.type.ArrayType;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.type.PrimitiveType;
import com.github.javaparser.ast.type.Type;
import com.webforj.component.Component;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

/**
 * Binds the types written in a source file to the classes the application has loaded.
 *
 * <p>
 * A type is looked up the way the compiler does, through the declarations of the file, its imports
 * and its package. No class is initialized and no application code runs.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class TypeResolver {

  private TypeResolver() {}

  /**
   * Loads a class of the application.
   *
   * @param name the binary class name
   *
   * @return the class, or {@code null} when the application cannot load it
   */
  public static Class<?> load(String name) {
    if (name == null || name.isEmpty()) {
      return null;
    }

    Class<?> type = loadWith(Thread.currentThread().getContextClassLoader(), name);

    return type != null ? type : loadWith(TypeResolver.class.getClassLoader(), name);
  }

  /**
   * Loads a class the way another class sees it.
   *
   * @param name the binary class name
   * @param from the class whose loader looks the name up
   *
   * @return the class, or {@code null} when that loader cannot load it
   */
  public static Class<?> load(String name, Class<?> from) {
    if (name == null || name.isEmpty() || from == null) {
      return null;
    }

    // A class of the platform names no loader
    ClassLoader loader = from.getClassLoader();

    return loadWith(loader == null ? ClassLoader.getPlatformClassLoader() : loader, name);
  }

  /**
   * Resolves a written type.
   *
   * @param type the type as the source names it
   * @param at the place the type is written at
   *
   * @return the class, or {@code null} when it cannot be resolved
   */
  public static Class<?> resolve(Type type, Node at) {
    if (type instanceof PrimitiveType primitive) {
      return resolvePrimitive(primitive);
    }

    if (type instanceof ArrayType array) {
      Class<?> component = resolve(array.getComponentType(), at);

      return component == null ? null : Array.newInstance(component, 0).getClass();
    }

    if (!(type instanceof ClassOrInterfaceType named)) {
      return null;
    }

    TypeDeclaration<?> written = findDeclaredType(named, at);
    if (written != null) {
      return resolve(written);
    }

    for (String candidate : getCandidates(named.getNameWithScope(), at)) {
      Class<?> loaded = loadNested(candidate);
      if (loaded != null) {
        return loaded;
      }
    }

    return null;
  }

  /**
   * Resolves a type by its name.
   *
   * @param name the simple or the fully qualified name
   * @param at the place the name is looked up from
   *
   * @return the class, or {@code null} when it cannot be resolved
   */
  public static Class<?> resolve(String name, Node at) {
    if (name == null || name.isEmpty()) {
      return null;
    }

    int generic = name.indexOf('<');
    String[] names = (generic < 0 ? name : name.substring(0, generic)).split("\\.");
    ClassOrInterfaceType type = null;
    for (String part : names) {
      type = new ClassOrInterfaceType(type, part);
    }

    return resolve(type, at);
  }

  /**
   * Resolves a type the source declares.
   *
   * @param type the type declaration
   *
   * @return the class of the declaration, the class of its nearest ancestor when the application
   *         did not load the declared class itself, or {@code null} when neither resolves
   */
  public static Class<?> resolve(TypeDeclaration<?> type) {
    Class<?> loaded = load(getBinaryName(type));
    if (loaded != null || !(type instanceof ClassOrInterfaceDeclaration declared)) {
      return loaded;
    }

    for (ClassOrInterfaceType extended : declared.getExtendedTypes()) {
      Class<?> ancestor = resolve(extended, type);
      if (ancestor != null) {
        return ancestor;
      }
    }

    return null;
  }

  /**
   * Resolves the type of an expression.
   *
   * @param expression the expression
   *
   * @return the class the expression is declared to produce, the named class itself for a type
   *         name, or {@code null} when it cannot be resolved
   */
  public static Class<?> resolveType(Expression expression) {
    return switch (expression) {
      case StringLiteralExpr ignored -> String.class;
      case TextBlockLiteralExpr ignored -> String.class;
      case CharLiteralExpr ignored -> char.class;
      case IntegerLiteralExpr ignored -> int.class;
      case LongLiteralExpr ignored -> long.class;
      case BooleanLiteralExpr ignored -> boolean.class;
      case InstanceOfExpr ignored -> boolean.class;
      case ClassExpr ignored -> Class.class;
      case DoubleLiteralExpr literal -> resolveDecimal(literal);
      case EnclosedExpr enclosed -> resolveType(enclosed.getInner());
      case CastExpr cast -> resolve(cast.getType(), cast);
      case ObjectCreationExpr creation -> resolve(creation.getType(), creation);
      case ThisExpr self -> resolveThis(self);
      case SuperExpr parent -> resolveParent(parent);
      case NameExpr name -> resolveName(name);
      case FieldAccessExpr access -> resolveField(access);
      case MethodCallExpr call -> MethodResolver.resolveReturnType(call);
      case ConditionalExpr choice -> resolveChoice(choice);
      case BinaryExpr operation -> resolveOperation(operation);
      default -> null;
    };
  }

  /**
   * Resolves the class a node is written in.
   *
   * @param node the node
   *
   * @return the class of the enclosing type, or {@code null} when it cannot be resolved
   */
  public static Class<?> resolveEnclosing(Node node) {
    Node type = VariableResolver.findEnclosingType(node);
    if (type instanceof TypeDeclaration<?> declared) {
      return resolve(declared);
    }

    return type instanceof ObjectCreationExpr creation ? resolve(creation.getType(), creation)
        : null;
  }

  /**
   * Gets the binary name of a type the source declares.
   *
   * @param type the type declaration
   *
   * @return the name the class loader knows the type by, {@code com.example.View$Inner} for a
   *         nested type
   */
  public static String getBinaryName(TypeDeclaration<?> type) {
    StringBuilder name = new StringBuilder(type.getNameAsString());
    Node parent = type.getParentNode().orElse(null);
    while (parent != null && !(parent instanceof CompilationUnit)) {
      if (parent instanceof TypeDeclaration<?> outer) {
        name.insert(0, outer.getNameAsString() + "$");
      }

      parent = parent.getParentNode().orElse(null);
    }

    String owner = parent instanceof CompilationUnit cu
        ? cu.getPackageDeclaration().map(declared -> declared.getNameAsString() + ".").orElse("")
        : "";

    return owner + name;
  }

  /**
   * Checks whether a class is the class of a name, extends it or implements it.
   *
   * @param type the class
   * @param name the binary name of the wanted class
   *
   * @return {@code true} when the wanted class, as the loader of the class knows it, is the class
   *         itself or one of its ancestors
   */
  public static boolean isInstance(Class<?> type, String name) {
    Class<?> wanted = load(name, type);

    return wanted != null && wanted.isAssignableFrom(type);
  }

  /**
   * Checks whether a class is a component.
   *
   * @param type the class
   *
   * @return {@code true} for a component class, as the loader of the class knows it since the
   *         application and the tool may load classes through different loaders
   */
  public static boolean isComponent(Class<?> type) {
    return isInstance(type, Component.class.getName());
  }

  /**
   * Resolves the parent of the class a node is written in.
   *
   * @param node the node
   *
   * @return the class the enclosing class extends, or {@code null} when it cannot be resolved
   */
  static Class<?> resolveParent(Node node) {
    Class<?> type = resolveEnclosing(node);
    Node written = VariableResolver.findEnclosingType(node);
    if (type == null || !(written instanceof TypeDeclaration<?> declared)) {
      return null;
    }

    // An enclosing class the application did not load was resolved to its ancestor already
    return load(getBinaryName(declared)) == null ? type : type.getSuperclass();
  }

  /**
   * Finds the declaration of a written type inside its own file.
   *
   * @param type the type as the source names it
   * @param at the place the type is written at
   *
   * @return the declaration, or {@code null} when the file does not declare the type
   */
  static TypeDeclaration<?> findDeclaredType(ClassOrInterfaceType type, Node at) { // NOSONAR
    CompilationUnit cu = at.findCompilationUnit().orElse(null);
    if (cu == null) {
      return null;
    }

    String written = type.getNameWithScope();
    for (TypeDeclaration<?> candidate : cu.findAll(TypeDeclaration.class)) {
      String declared = getBinaryName(candidate).replace('$', '.');
      if (declared.equals(written) || declared.endsWith("." + written)) {
        return candidate;
      }
    }

    return null;
  }

  // The order the compiler looks a name up in, the name itself, the imports that name it, the
  // package of the file, the classes every file knows and the imports of whole packages
  private static List<String> getCandidates(String written, Node at) {
    int dot = written.indexOf('.');
    String first = dot < 0 ? written : written.substring(0, dot);
    String rest = dot < 0 ? "" : written.substring(dot);
    CompilationUnit cu = at.findCompilationUnit().orElse(null);
    List<ImportDeclaration> imports = cu == null ? List.of()
        : cu.getImports().stream().filter(declaration -> !declaration.isStatic()).toList();

    List<String> candidates = new ArrayList<>();
    candidates.add(written);
    imports.stream()
        .filter(declaration -> !declaration.isAsterisk()
            && declaration.getName().getIdentifier().equals(first))
        .forEach(declaration -> candidates.add(declaration.getNameAsString() + rest));
    if (cu != null) {
      cu.getPackageDeclaration()
          .ifPresent(declared -> candidates.add(declared.getNameAsString() + "." + written));
    }

    candidates.add("java.lang." + written);
    imports.stream().filter(ImportDeclaration::isAsterisk)
        .forEach(declaration -> candidates.add(declaration.getNameAsString() + "." + written));

    return candidates;
  }

  // The loader separates a nested class from its owner by a dollar, the source by a dot
  private static Class<?> loadNested(String name) {
    String candidate = name;
    while (true) {
      Class<?> loaded = load(candidate);
      int dot = candidate.lastIndexOf('.');
      if (loaded != null || dot < 0) {
        return loaded;
      }

      candidate = candidate.substring(0, dot) + "$" + candidate.substring(dot + 1);
    }
  }

  private static Class<?> resolvePrimitive(PrimitiveType type) {
    return switch (type.getType()) {
      case BOOLEAN -> boolean.class;
      case CHAR -> char.class;
      case BYTE -> byte.class;
      case SHORT -> short.class;
      case INT -> int.class;
      case LONG -> long.class;
      case FLOAT -> float.class;
      case DOUBLE -> double.class;
    };
  }

  private static Class<?> resolveDecimal(DoubleLiteralExpr literal) {
    String value = literal.getValue();

    return value.endsWith("f") || value.endsWith("F") ? float.class : double.class;
  }

  private static Class<?> resolveThis(ThisExpr self) {
    return self.getTypeName().<Class<?>>map(name -> resolve(name.asString(), self))
        .orElseGet(() -> resolveEnclosing(self));
  }

  private static Class<?> resolveName(NameExpr name) {
    Node declaration = VariableResolver.findDeclaration(name);
    if (declaration instanceof VariableDeclarator variable) {
      return resolveVariable(variable);
    }

    if (declaration instanceof Parameter parameter) {
      return resolve(parameter.getType(), parameter);
    }

    return resolve(new ClassOrInterfaceType(null, name.getNameAsString()), name);
  }

  private static Class<?> resolveVariable(VariableDeclarator variable) {
    if (!variable.getType().isVarType()) {
      return resolve(variable.getType(), variable);
    }

    return variable.getInitializer().map(TypeResolver::resolveType).orElse(null);
  }

  private static Class<?> resolveField(FieldAccessExpr access) {
    Node declaration = VariableResolver.findDeclaration(access);
    if (declaration instanceof VariableDeclarator variable) {
      return resolveVariable(variable);
    }

    Class<?> owner = resolveType(access.getScope());
    Class<?> field = owner == null ? null : findFieldType(owner, access.getNameAsString());
    if (field != null) {
      return field;
    }

    // A name of several parts that names no field is a qualified type
    return loadNested(access.toString());
  }

  private static Class<?> findFieldType(Class<?> owner, String name) {
    try {
      for (Field field : owner.getFields()) {
        if (field.getName().equals(name)) {
          return field.getType();
        }
      }

      for (Class<?> current = owner; current != null; current = current.getSuperclass()) {
        for (Field field : current.getDeclaredFields()) {
          if (field.getName().equals(name)) {
            return field.getType();
          }
        }
      }
    } catch (LinkageError | SecurityException e) {
      return null;
    }

    return null;
  }

  private static Class<?> resolveChoice(ConditionalExpr choice) {
    Class<?> first = resolveType(choice.getThenExpr());
    Class<?> second = resolveType(choice.getElseExpr());

    return first != null && first.equals(second) ? first : null;
  }

  private static Class<?> resolveOperation(BinaryExpr operation) {
    return switch (operation.getOperator()) {
      case OR, AND, EQUALS, NOT_EQUALS, LESS, GREATER, LESS_EQUALS, GREATER_EQUALS -> boolean.class;
      case PLUS -> resolveType(operation.getLeft()) == String.class
          || resolveType(operation.getRight()) == String.class ? String.class : null;
      default -> null;
    };
  }

  private static Class<?> loadWith(ClassLoader loader, String name) {
    if (loader == null) {
      return null;
    }

    try {
      return Class.forName(name, false, loader);
    } catch (ClassNotFoundException | LinkageError e) {
      return null;
    }
  }
}
