package com.webforj.devtools.craftforj.inspector.source.generator;

import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.NullLiteralExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.expr.TextBlockLiteralExpr;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;

/**
 * Source generator for scalar values (String, Boolean, Integer, etc.).
 *
 * <p>
 * Uses the property's javaType to format literals correctly. This ensures that values from the
 * client (which sends all numbers as Double) are converted to the correct Java type:
 * </p>
 * <ul>
 * <li>javaType=Integer.class, value=2.0 → generates "2" (not "2.0")</li>
 * <li>javaType=Double.class, value=2.0 → generates "2.0"</li>
 * </ul>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class ScalarSourceGenerator implements SourceGenerator {

  /**
   * {@inheritDoc}
   */
  @Override
  public SourceChange generate(GeneratorContext context) {
    Object value = context.getValue();

    // Empty/null means "remove" - return null to trigger method call removal
    if (value == null || (value instanceof String s && s.isEmpty())) {
      return null;
    }

    try {
      Expression expr = toExpression(value, context.getJavaType());
      return SourceChange.builder().methodCall(context.getMethodName(), expr).build();
    } catch (SourceModificationException e) {
      throw new SourceModificationException(
          "Property '" + context.getMethodName() + "': " + e.getMessage());
    }
  }

  /**
   * Converts a value to a JavaParser expression using the expected Java type.
   *
   * <p>
   * The javaType parameter ensures correct literal formatting. JavaScript sends all numbers as
   * Double, but this method converts to Integer when javaType indicates Integer.class.
   * </p>
   *
   * @param value the value to convert
   * @param javaType the expected Java type (may be null, in which case value type is used)
   * @return the JavaParser expression
   */
  public static Expression toExpression(Object value, Class<?> javaType) {
    if (value == null) {
      return new NullLiteralExpr();
    }

    // Use javaType to determine correct literal format
    if (javaType != null) {
      Expression expr = switch (javaType.getName()) {
        case "java.lang.Integer", "int" -> {
          int intValue =
              (value instanceof Number n) ? n.intValue() : Integer.parseInt(value.toString());
          yield new IntegerLiteralExpr(String.valueOf(intValue));
        }
        case "java.lang.Long", "long" -> {
          long longValue =
              (value instanceof Number n) ? n.longValue() : Long.parseLong(value.toString());
          yield new IntegerLiteralExpr(longValue + "L");
        }
        case "java.lang.Double", "double" -> {
          double doubleValue =
              (value instanceof Number n) ? n.doubleValue() : Double.parseDouble(value.toString());
          yield new DoubleLiteralExpr(doubleValue);
        }
        case "java.lang.Float", "float" -> {
          float floatValue =
              (value instanceof Number n) ? n.floatValue() : Float.parseFloat(value.toString());
          yield new DoubleLiteralExpr(floatValue + "f");
        }
        case "java.lang.Boolean", "boolean" -> {
          boolean boolValue =
              (value instanceof Boolean b) ? b : Boolean.parseBoolean(value.toString());
          yield new BooleanLiteralExpr(boolValue);
        }
        case "java.lang.String" -> stringExpression(value.toString());
        default -> null;
      };
      if (expr != null) {
        return expr;
      }
    }

    // Fall back to value type inference

    return toExpression(value);
  }

  /**
   * Converts a value to a JavaParser expression based on its runtime type.
   *
   * <p>
   * This method is used as fallback when no javaType is specified, and by other generators (like
   * ListSourceGenerator) that work with individual items.
   * </p>
   *
   * @param value the value to convert
   * @return the JavaParser expression
   */
  public static Expression toExpression(Object value) {
    if (value == null) {
      return new NullLiteralExpr();
    }
    if (value instanceof String s) {
      return new StringLiteralExpr().setString(s);
    }
    if (value instanceof Boolean b) {
      return new BooleanLiteralExpr(b);
    }
    if (value instanceof Integer i) {
      return new IntegerLiteralExpr(String.valueOf(i));
    }
    if (value instanceof Long l) {
      return new IntegerLiteralExpr(String.valueOf(l) + "L");
    }
    if (value instanceof Double d) {
      return new DoubleLiteralExpr(d);
    }
    if (value instanceof Float f) {
      return new DoubleLiteralExpr(String.valueOf(f) + "f");
    }
    throw new SourceModificationException("Unsupported value type: " + value.getClass().getName());
  }

  /**
   * Converts a string value to the most readable literal form.
   *
   * <p>
   * Multiline values become text blocks so the generated source stays readable; values with
   * carriage returns fall back to a single-line literal because text blocks normalize line
   * terminators and would corrupt them. The single-line path uses {@code setString}, which escapes
   * quotes and backslashes, whereas the {@code StringLiteralExpr} constructor only escapes EOL
   * chars.
   * </p>
   *
   * @param value the string value
   * @return the JavaParser expression
   */
  public static Expression stringExpression(String value) {
    if (value.contains("\n") && !value.contains("\r")) {
      return new TextBlockLiteralExpr(toTextBlockContent(value));
    }

    return new StringLiteralExpr().setString(value);
  }

  private static final String TEXT_BLOCK_INDENT = "        ";

  /**
   * Builds the raw text-block content whose compiled value equals the given string.
   *
   * <p>
   * Every content line and the closing delimiter share the same indent, so incidental-whitespace
   * stripping removes exactly that indent and nothing of the payload. A trailing backslash
   * continuation is added when the value itself does not end with a newline.
   * </p>
   *
   * @param value the string value
   * @return the raw content to store in a {@code TextBlockLiteralExpr}
   */
  private static String toTextBlockContent(String value) {
    String escaped = value.replace("\\", "\\\\").replace("\"\"\"", "\\\"\\\"\\\"");
    boolean endsWithNewline = escaped.endsWith("\n");
    String[] lines = escaped.split("\n", -1);
    int count = endsWithNewline ? lines.length - 1 : lines.length;
    StringBuilder content = new StringBuilder();

    for (int i = 0; i < count; i++) {
      String line = lines[i];
      if (line.endsWith(" ")) {
        line = line.substring(0, line.length() - 1) + "\\s";
      } else if (line.endsWith("\t")) {
        line = line.substring(0, line.length() - 1) + "\\t";
      }
      if (i == count - 1 && !endsWithNewline) {
        line = line + "\\";
      }
      if (!line.isEmpty()) {
        content.append(TEXT_BLOCK_INDENT);
      }
      content.append(line).append('\n');
    }
    content.append(TEXT_BLOCK_INDENT);

    return content.toString();
  }
}
