package com.webforj.devtools.craftforj.inspector.source.generator;

import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;

/**
 * Source generator for enum values.
 *
 * <p>
 * Parses fully qualified enum values and adds required imports.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class EnumSourceGenerator implements SourceGenerator {

  private final Class<?> knownEnumClass;

  /**
   * Creates a generator that resolves the enum class from the client value.
   */
  public EnumSourceGenerator() {
    this(null);
  }

  /**
   * Creates a generator with the enum class already resolved server-side.
   *
   * @param knownEnumClass the enum class resolved from the component, or {@code null} to resolve
   *        from the client value
   */
  public EnumSourceGenerator(Class<?> knownEnumClass) {
    this.knownEnumClass = knownEnumClass;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings({"unchecked", "rawtypes"})
  public SourceChange generate(GeneratorContext context) {
    String value = context.getValue() != null ? context.getValue().toString() : null;
    if (value == null || value.isEmpty()) {
      throw new SourceModificationException(
          "Property '" + context.getMethodName() + "' requires a value");
    }

    int lastDot = value.lastIndexOf('.');
    if (lastDot < 0) {
      throw new SourceModificationException(
          "Property '" + context.getMethodName() + "' has invalid enum format: " + value);
    }

    String enumClassName = value.substring(0, lastDot);
    String enumName = value.substring(lastDot + 1);

    try {
      Class<?> enumClass = knownEnumClass != null ? knownEnumClass : loadEnumClass(enumClassName);
      if (!enumClass.isEnum()) {
        throw new SourceModificationException(
            "Property '" + context.getMethodName() + "': " + enumClassName + " is not an enum");
      }

      Enum<?> enumValue = Enum.valueOf((Class<Enum>) enumClass, enumName);

      return SourceChange.builder()
          .methodCall(context.getMethodName(),
              new FieldAccessExpr(new NameExpr(enumClass.getSimpleName()), enumValue.name()))
          .addImport(enumClass.getCanonicalName()).build();
    } catch (ClassNotFoundException e) {
      throw new SourceModificationException(
          "Property '" + context.getMethodName() + "': enum class not found: " + enumClassName);
    } catch (IllegalArgumentException e) {
      throw new SourceModificationException(
          "Property '" + context.getMethodName() + "': invalid enum value '" + enumName + "'");
    }
  }

  /**
   * Loads an enum class by name, handling both canonical (dots) and internal ($) formats.
   *
   * <p>
   * For nested classes, Class.forName() requires the internal format with $ separators (e.g.,
   * "OuterClass$InnerClass"), but values from the client may use canonical format with dots (e.g.,
   * "OuterClass.InnerClass"). This method tries both formats. Classes are loaded without
   * initialization, so a client-supplied name cannot run static initializers; the enum check in the
   * caller runs before anything triggers initialization.
   * </p>
   */
  private Class<?> loadEnumClass(String className) throws ClassNotFoundException {
    try {
      // Try as-is first (handles both $ format and non-nested classes)
      return loadWithoutInit(className);
    } catch (ClassNotFoundException e) {
      // For nested classes with canonical format, try converting last segments to $
      // e.g., "com.pkg.Outer.Inner" -> "com.pkg.Outer$Inner"
      int lastDot = className.lastIndexOf('.');
      while (lastDot > 0) {
        String withDollar =
            className.substring(0, lastDot) + "$" + className.substring(lastDot + 1);
        try {
          return loadWithoutInit(withDollar);
        } catch (ClassNotFoundException ignored) {
          // Try next level up
          className = withDollar;
          lastDot = className.lastIndexOf('.');
        }
      }
      throw e; // Re-throw original if none worked
    }
  }

  private Class<?> loadWithoutInit(String className) throws ClassNotFoundException {
    return Class.forName(className, false, getClass().getClassLoader());
  }
}
