package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.component.layout.columnslayout.ColumnsLayout.Breakpoint;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator.GeneratorContext;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ColumnsLayoutBreakpointsContributionTest {

  private final ColumnsLayoutBreakpointsContribution contribution =
      new ColumnsLayoutBreakpointsContribution();

  private static Map<String, Object> entry(String name, Object minWidth, int columns) {
    Map<String, Object> map = new HashMap<>();
    map.put(ColumnsLayoutBreakpointsContribution.KEY_NAME, name);
    map.put(ColumnsLayoutBreakpointsContribution.KEY_MIN_WIDTH, minWidth);
    map.put(ColumnsLayoutBreakpointsContribution.KEY_COLUMNS, columns);

    return map;
  }

  @Test
  void shouldSupportColumnsLayout() {
    ColumnsLayout layout = mock(ColumnsLayout.class);

    assertTrue(contribution.supports(layout));
  }

  @Test
  void shouldGetBreakpointsAsListOfMaps() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    when(layout.getBreakpoints()).thenReturn(
        List.of(new Breakpoint("small", "20em", 1), new Breakpoint("medium", "40em", 2)));

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("Breakpoints", result.get().getName());
    assertEquals(PropertyType.LIST, result.get().getEditorType());
    assertEquals(Boolean.TRUE, result.get().getEditorConfig().get("hidden"));

    @SuppressWarnings("unchecked")
    var value = (List<Map<String, Object>>) result.get().getValue();
    assertEquals(2, value.size());
    assertEquals("small", value.get(0).get(ColumnsLayoutBreakpointsContribution.KEY_NAME));
    assertEquals("20em", value.get(0).get(ColumnsLayoutBreakpointsContribution.KEY_MIN_WIDTH));
    assertEquals(1, value.get(0).get(ColumnsLayoutBreakpointsContribution.KEY_COLUMNS));
  }

  @Test
  void shouldReturnEmptyBreakpointsWhenGetterReturnsNull() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    when(layout.getBreakpoints()).thenReturn(null);

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals(List.of(), result.get().getValue());
  }

  @Test
  void shouldSetBreakpointsFromListOfMaps() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    List<Map<String, Object>> value =
        List.of(entry("small", "20em", 1), entry("medium", "40em", 2));

    assertTrue(contribution.set(layout, value));
    verify(layout).setBreakpoints(
        List.of(new Breakpoint("small", "20em", 1), new Breakpoint("medium", "40em", 2)));
  }

  @Test
  void shouldConvertNumericMinWidthToString() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    List<Map<String, Object>> value = List.of(entry("small", 20.0, 1));

    assertTrue(contribution.set(layout, value));
    verify(layout).setBreakpoints(List.of(new Breakpoint("small", "20", 1)));
  }

  @Test
  void shouldFallBackNameToMinWidthWhenBlank() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    List<Map<String, Object>> value = List.of(entry("   ", "20em", 1));

    assertTrue(contribution.set(layout, value));
    verify(layout).setBreakpoints(List.of(new Breakpoint("20em", "20em", 1)));
  }

  @Test
  void shouldSkipRowsMissingMinWidthOrColumns() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    Map<String, Object> missingMinWidth = new HashMap<>();
    missingMinWidth.put(ColumnsLayoutBreakpointsContribution.KEY_NAME, "small");
    missingMinWidth.put(ColumnsLayoutBreakpointsContribution.KEY_COLUMNS, 1);
    List<Map<String, Object>> value =
        List.of(missingMinWidth, entry("medium", "40em", 0), entry("large", "60em", 3));

    assertTrue(contribution.set(layout, value));
    verify(layout).setBreakpoints(List.of(new Breakpoint("large", "60em", 3)));
  }

  @Test
  void shouldFallBackToDefaultBreakpointsWhenAllRowsInvalid() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    List<Map<String, Object>> value = List.of(entry("bad", null, 0));

    assertTrue(contribution.set(layout, value));
    verify(layout).setBreakpoints(ColumnsLayout.DEFAULT_BREAKPOINTS);
  }

  @Test
  void shouldSkipRowWithNonNumericColumns() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    Map<String, Object> nonNumericColumns = new HashMap<>();
    nonNumericColumns.put(ColumnsLayoutBreakpointsContribution.KEY_NAME, "small");
    nonNumericColumns.put(ColumnsLayoutBreakpointsContribution.KEY_MIN_WIDTH, "20em");
    nonNumericColumns.put(ColumnsLayoutBreakpointsContribution.KEY_COLUMNS, "abc");
    List<Map<String, Object>> value = List.of(nonNumericColumns);

    assertTrue(contribution.set(layout, value));
    verify(layout).setBreakpoints(ColumnsLayout.DEFAULT_BREAKPOINTS);
  }

  @Test
  void shouldFallBackToDefaultBreakpointsForNullValue() {
    ColumnsLayout layout = mock(ColumnsLayout.class);

    assertTrue(contribution.set(layout, null));
    verify(layout).setBreakpoints(ColumnsLayout.DEFAULT_BREAKPOINTS);
  }

  @Test
  void shouldFallBackToDefaultBreakpointsForEmptyList() {
    ColumnsLayout layout = mock(ColumnsLayout.class);

    assertTrue(contribution.set(layout, List.of()));
    verify(layout).setBreakpoints(ColumnsLayout.DEFAULT_BREAKPOINTS);
  }

  @Test
  void shouldExposeBreakpointsSourceGenerator() {
    SourceGenerator generator = contribution.getSourceGenerator();

    assertTrue(
        generator instanceof ColumnsLayoutBreakpointsContribution.BreakpointsSourceGenerator);
  }

  @Nested
  class BreakpointsSourceGeneratorTests {

    private final ColumnsLayoutBreakpointsContribution.BreakpointsSourceGenerator generator =
        new ColumnsLayoutBreakpointsContribution.BreakpointsSourceGenerator();

    private GeneratorContext ctx(Object value) {
      FeatureProperty property = FeatureProperty.builder("Breakpoints", "ColumnsLayoutBreakpoints")
          .list().hidden().value(value).build();

      return new GeneratorContext("setBreakpoints", property);
    }

    @Test
    void shouldReturnNullForEmptyList() {
      assertNull(generator.generate(ctx(List.of())));
    }

    @Test
    void shouldReturnNullForNonListValue() {
      assertNull(generator.generate(ctx("oops")));
    }

    @Test
    void shouldReturnNullWhenListHasNoMapEntries() {
      assertNull(generator.generate(ctx(List.of("not-a-map"))));
    }

    @Test
    void shouldReturnNullForNullValue() {
      assertNull(generator.generate(ctx(null)));
    }

    @Test
    void shouldGenerateListOfBreakpointsWithImports() {
      SourceChange change =
          generator.generate(ctx(List.of(entry("small", "20px", 1), entry("medium", "50%", 2))));

      assertEquals("setBreakpoints", change.getMethodName());
      assertEquals(2, change.getImports().size());
      assertTrue(change.getImports().contains(List.class.getName()));
      assertTrue(change.getImports().contains(Breakpoint.class.getCanonicalName()));

      Expression argument = change.getArgument();
      assertTrue(argument instanceof MethodCallExpr);
      MethodCallExpr call = (MethodCallExpr) argument;
      assertEquals("of", call.getNameAsString());
      assertEquals("List", call.getScope().orElseThrow().toString());

      NodeList<Expression> entries = call.getArguments();
      assertEquals(2, entries.size());

      ObjectCreationExpr first = (ObjectCreationExpr) entries.get(0);
      assertEquals("Breakpoint", first.getType().getNameAsString());
      assertEquals("small", ((StringLiteralExpr) first.getArguments().get(0)).asString());
      assertTrue(first.getArguments().get(1) instanceof IntegerLiteralExpr);
      assertEquals("20", first.getArguments().get(1).toString());
      assertTrue(first.getArguments().get(2) instanceof IntegerLiteralExpr);
      assertEquals("1", first.getArguments().get(2).toString());

      ObjectCreationExpr second = (ObjectCreationExpr) entries.get(1);
      assertEquals("medium", ((StringLiteralExpr) second.getArguments().get(0)).asString());
      assertTrue(second.getArguments().get(1) instanceof StringLiteralExpr);
      assertEquals("50%", ((StringLiteralExpr) second.getArguments().get(1)).asString());
    }

    @Test
    void shouldDefaultMissingMinWidthToZeroLiteral() {
      SourceChange change = generator.generate(ctx(List.of(entry("small", null, 1))));

      ObjectCreationExpr creation =
          (ObjectCreationExpr) change.getArgument().asMethodCallExpr().getArguments().get(0);
      assertTrue(creation.getArguments().get(1) instanceof IntegerLiteralExpr);
      assertEquals("0", creation.getArguments().get(1).toString());
    }

    @Test
    void shouldFallBackNameExpressionToMinWidthWhenNull() {
      SourceChange change = generator.generate(ctx(List.of(entry(null, "20em", 1))));

      ObjectCreationExpr creation =
          (ObjectCreationExpr) change.getArgument().asMethodCallExpr().getArguments().get(0);
      assertEquals("20em", ((StringLiteralExpr) creation.getArguments().get(0)).asString());
    }
  }
}
