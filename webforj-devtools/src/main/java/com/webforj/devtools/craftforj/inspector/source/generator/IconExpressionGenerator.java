package com.webforj.devtools.craftforj.inspector.source.generator;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.component.icons.DwcIcon;
import com.webforj.component.icons.FeatherIcon;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import java.util.List;
import java.util.Locale;
import java.util.function.Predicate;

/**
 * Builds the canonical icon factory expression for a {@code "pool:name"} icon value.
 *
 * <p>
 * Known pools produce their idiomatic factory call: {@code TablerIcon.create("home")},
 * {@code FeatherIcon.BELL.create()}, {@code DwcIcon.CALENDAR.create()} and
 * {@code FontAwesomeIcon.create("star")}. Any other pool, or an icon name with no matching enum
 * constant, falls back to the generic {@code new Icon("name", "pool")} form which is valid for
 * every pool.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class IconExpressionGenerator {

  private static final String ICONS_PACKAGE = "com.webforj.component.icons.";

  /**
   * An icon expression with the imports it requires.
   *
   * @param expression the icon factory expression
   * @param imports the fully qualified names the expression references
   */
  public record IconExpression(Expression expression, List<String> imports) {}

  /**
   * A parsed icon value.
   *
   * @param pool the icon pool
   * @param name the icon name
   */
  public record IconValue(String pool, String name) {}

  private IconExpressionGenerator() {}

  /**
   * Parses a {@code "pool:name"} icon value.
   *
   * @param value the icon value
   *
   * @return the parsed pool and name
   * @throws SourceModificationException if the value is not in {@code "pool:name"} format
   */
  public static IconValue parseValue(Object value) {
    String text = value == null ? "" : String.valueOf(value).trim();
    int separator = text.indexOf(':');
    if (separator <= 0 || separator == text.length() - 1) {
      throw new SourceModificationException("Icon value must be in 'pool:name' format: " + value);
    }

    return new IconValue(text.substring(0, separator), text.substring(separator + 1));
  }

  /**
   * Generates the icon factory expression for the given {@code "pool:name"} value.
   *
   * @param value the icon value in {@code "pool:name"} format
   *
   * @return the expression and its required imports
   * @throws SourceModificationException if the value is not in {@code "pool:name"} format
   */
  public static IconExpression generate(String value) {
    IconValue parsed = parseValue(value);
    String pool = parsed.pool();
    String name = parsed.name();

    return switch (pool) {
      case "tabler" -> staticFactory("TablerIcon", name);
      case "fa" -> staticFactory("FontAwesomeIcon", name);
      case "feather" -> enumFactory("FeatherIcon", name, pool, IconExpressionGenerator::isFeather);
      case "dwc" -> enumFactory("DwcIcon", name, pool, IconExpressionGenerator::isDwc);
      default -> generic(name, pool);
    };
  }

  private static IconExpression staticFactory(String className, String name) {
    MethodCallExpr call = new MethodCallExpr(new NameExpr(className), "create",
        NodeList.nodeList(new StringLiteralExpr(name)));

    return new IconExpression(call, List.of(ICONS_PACKAGE + className));
  }

  private static IconExpression enumFactory(String className, String name, String pool,
      Predicate<String> exists) {
    String constant = name.toUpperCase(Locale.ENGLISH).replace('-', '_');
    if (!exists.test(constant)) {
      return generic(name, pool);
    }

    MethodCallExpr call = new MethodCallExpr(new FieldAccessExpr(new NameExpr(className), constant),
        "create", new NodeList<>());

    return new IconExpression(call, List.of(ICONS_PACKAGE + className));
  }

  private static IconExpression generic(String name, String pool) {
    ObjectCreationExpr creation =
        new ObjectCreationExpr(null, StaticJavaParser.parseClassOrInterfaceType("Icon"),
            NodeList.nodeList(new StringLiteralExpr(name), new StringLiteralExpr(pool)));

    return new IconExpression(creation, List.of(ICONS_PACKAGE + "Icon"));
  }

  private static boolean isFeather(String constant) {
    try {
      FeatherIcon.valueOf(constant);
      return true;
    } catch (IllegalArgumentException e) {
      return false;
    }
  }

  private static boolean isDwc(String constant) {
    try {
      DwcIcon.valueOf(constant);
      return true;
    } catch (IllegalArgumentException e) {
      return false;
    }
  }
}
