package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.google.auto.service.AutoService;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.component.layout.columnslayout.ColumnsLayout.Breakpoint;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator;
import com.webforj.devtools.craftforj.source.model.SourceChange;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Contribution for the responsive breakpoints of a columns layout.
 *
 * <p>
 * The value is a list of {@code {name, minWidth, columns}} entries mirroring {@link Breakpoint}.
 * The property is hidden from the generic editor rows and edited through the dedicated columns
 * layout visual editor. Generated source uses the ColumnsLayout API:
 * {@code layout.setBreakpoints(List.of(new Breakpoint("small", "20em", 1), ...))}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ColumnsLayoutBreakpointsContribution extends ConcernContribution<ColumnsLayout> {

  static final String KEY_NAME = "name";
  static final String KEY_MIN_WIDTH = "minWidth";
  static final String KEY_COLUMNS = "columns";

  /** Creates the columns layout breakpoints contribution. */
  public ColumnsLayoutBreakpointsContribution() {
    super(ColumnsLayout.class, "Breakpoints", FeatureCategory.LAYOUT);

    setBuilderConfig(b -> b.list().hidden());
    setGetter(ColumnsLayoutBreakpointsContribution::readBreakpoints);
    setSetter((layout, value) -> layout.setBreakpoints(parseBreakpoints(value)));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public SourceGenerator getSourceGenerator() {
    return new BreakpointsSourceGenerator();
  }

  private static List<Map<String, Object>> readBreakpoints(ColumnsLayout layout) {
    List<Breakpoint> breakpoints = layout.getBreakpoints();
    if (breakpoints == null) {
      return List.of();
    }

    List<Map<String, Object>> result = new ArrayList<>();

    for (Breakpoint breakpoint : breakpoints) {
      Map<String, Object> entry = new LinkedHashMap<>();
      entry.put(KEY_NAME, breakpoint.name());
      entry.put(KEY_MIN_WIDTH, breakpoint.minWidth());
      entry.put(KEY_COLUMNS, breakpoint.columns());
      result.add(entry);
    }

    return result;
  }

  private static List<Breakpoint> parseBreakpoints(Object value) {
    List<Breakpoint> breakpoints = parseValidBreakpoints(value);
    return breakpoints.isEmpty() ? ColumnsLayout.DEFAULT_BREAKPOINTS : breakpoints;
  }

  private static List<Breakpoint> parseValidBreakpoints(Object value) {
    if (!(value instanceof List<?> list) || list.isEmpty()) {
      return List.of();
    }

    List<Breakpoint> breakpoints = new ArrayList<>();
    for (Object entry : list) {
      if (!(entry instanceof Map<?, ?> map)) {
        continue;
      }

      String name = stringValue(map.get(KEY_NAME));
      String minWidth = stringValue(map.get(KEY_MIN_WIDTH));
      int columns = intValue(map.get(KEY_COLUMNS));

      if (minWidth == null || columns < 1) {
        continue;
      }

      breakpoints.add(new Breakpoint(name == null || name.isBlank() ? minWidth : name.trim(),
          minWidth, columns));
    }

    return breakpoints;
  }

  private static String stringValue(Object value) {
    if (value == null) {
      return null;
    }

    if (value instanceof Number n && n.doubleValue() == Math.floor(n.doubleValue())) {
      return String.valueOf(n.intValue());
    }

    String text = String.valueOf(value).trim();

    return text.isEmpty() ? null : text;
  }

  private static int intValue(Object value) {
    if (value instanceof Number n) {
      return n.intValue();
    }

    try {
      return Integer.parseInt(String.valueOf(value).trim());
    } catch (NumberFormatException e) {
      return 0;
    }
  }

  /**
   * Generates {@code setBreakpoints(List.of(new Breakpoint(...), ...))} expressions.
   */
  static final class BreakpointsSourceGenerator implements SourceGenerator {

    /**
     * {@inheritDoc}
     */
    @Override
    public SourceChange generate(GeneratorContext context) {
      List<Breakpoint> breakpoints = parseValidBreakpoints(context.getValue());
      if (breakpoints.isEmpty()) {
        return null;
      }

      NodeList<Expression> entries = new NodeList<>();
      for (Breakpoint breakpoint : breakpoints) {
        entries.add(breakpointExpression(breakpoint));
      }

      MethodCallExpr listOf = new MethodCallExpr(new NameExpr("List"), "of", entries);

      return SourceChange.builder().methodCall(context.getMethodName(), listOf)
          .addImport(List.class.getName()).addImport(Breakpoint.class.getCanonicalName()).build();
    }

    private Expression breakpointExpression(Breakpoint breakpoint) {
      NodeList<Expression> args = new NodeList<>();
      args.add(new StringLiteralExpr().setString(breakpoint.name()));
      args.add(minWidthExpression(breakpoint.minWidth()));
      args.add(new IntegerLiteralExpr(String.valueOf(breakpoint.columns())));

      return new ObjectCreationExpr(null,
          StaticJavaParser.parseClassOrInterfaceType(Breakpoint.class.getSimpleName()), args);
    }

    private Expression minWidthExpression(String minWidth) {
      String value = minWidth == null ? "0" : minWidth;
      String pixels = value.endsWith("px") ? value.substring(0, value.length() - 2) : value;

      if (pixels.matches("\\d+")) {
        try {
          return new IntegerLiteralExpr(Integer.toString(Integer.parseInt(pixels)));
        } catch (NumberFormatException e) {
          // Wider CSS lengths use the String overload instead of an invalid Java int literal.
        }
      }

      return new StringLiteralExpr().setString(value);
    }
  }
}
