package com.webforj.devtools.craftforj.inspector.source.generator;

import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.NullLiteralExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.expr.TextBlockLiteralExpr;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceChange;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeParseException;

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
      SourceChange.Builder change =
          SourceChange.builder().methodCall(context.getMethodName(), expr).replaceAllCalls(true);
      Class<?> type = context.getJavaType();
      if (type == LocalDate.class || type == LocalTime.class || type == LocalDateTime.class) {
        change.addImport(type.getName());
      }
      return change.build();
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
        case "java.lang.Integer", "int" -> new IntegerLiteralExpr(String.valueOf(toInt(value)));
        case "java.lang.Long", "long" -> new IntegerLiteralExpr(toLong(value) + "L");
        case "java.lang.Double", "double" -> new DoubleLiteralExpr(toDouble(value));
        case "java.lang.Float", "float" -> new DoubleLiteralExpr(toFloat(value) + "f");
        case "java.lang.Boolean", "boolean" -> new BooleanLiteralExpr(toBoolean(value));
        case "java.lang.String" -> stringExpression(value.toString());
        case "java.time.LocalDate", "java.time.LocalTime", "java.time.LocalDateTime" ->
          toTemporalExpression(value, javaType);
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
      return new DoubleLiteralExpr(toDouble(d));
    }
    if (value instanceof Float f) {
      return new DoubleLiteralExpr(toFloat(f) + "f");
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

  private static Expression toTemporalExpression(Object value, Class<?> type) {
    try {
      String text = value.toString();
      String parsed = switch (type.getName()) {
        case "java.time.LocalDate" -> LocalDate.parse(text).toString();
        case "java.time.LocalTime" -> LocalTime.parse(text).toString();
        default -> LocalDateTime.parse(text).toString();
      };
      return new MethodCallExpr(new NameExpr(type.getSimpleName()), "parse")
          .addArgument(new StringLiteralExpr().setString(parsed));
    } catch (DateTimeParseException e) {
      throw new SourceModificationException(
          "expected a valid " + type.getSimpleName() + " value in ISO format");
    }
  }

  private static int toInt(Object value) {
    try {
      return new BigDecimal(value.toString()).intValueExact();
    } catch (NumberFormatException | ArithmeticException e) {
      throw new SourceModificationException("expected a whole number in the int range");
    }
  }

  private static long toLong(Object value) {
    try {
      return new BigDecimal(value.toString()).longValueExact();
    } catch (NumberFormatException | ArithmeticException e) {
      throw new SourceModificationException("expected a whole number in the long range");
    }
  }

  private static double toDouble(Object value) {
    try {
      double number = Double.parseDouble(value.toString());
      if (Double.isFinite(number)) {
        return number;
      }
    } catch (NumberFormatException e) {
      // Report the same property error for malformed and non-finite values.
    }
    throw new SourceModificationException("expected a finite double value");
  }

  private static float toFloat(Object value) {
    try {
      float number = Float.parseFloat(value.toString());
      if (Float.isFinite(number)) {
        return number;
      }
    } catch (NumberFormatException e) {
      // Report the same property error for malformed and non-finite values.
    }
    throw new SourceModificationException("expected a finite float value");
  }

  private static boolean toBoolean(Object value) {
    String text = value.toString();
    if ("true".equalsIgnoreCase(text) || "false".equalsIgnoreCase(text)) {
      return Boolean.parseBoolean(text);
    }
    throw new SourceModificationException("expected true or false");
  }

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
