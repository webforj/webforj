package com.webforj.devtools.craftforj.inspector.source.generator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator.GeneratorContext;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;

class ListSourceGeneratorTest {

  private final SourceGenerator generator = new ListSourceGenerator();

  private GeneratorContext ctx(String methodName, Object value) {
    FeatureProperty prop = FeatureProperty.builder("Prop", "Feature").list().value(value).build();
    return new GeneratorContext(methodName, prop);
  }

  @Test
  void shouldGenerateSingleStringArgument() {
    SourceChange change = generator.generate(ctx("addClassName", List.of("class1")));

    assertEquals("addClassName", change.getMethodName());
    assertEquals(1, change.getArguments().size());
    assertEquals("\"class1\"", change.getArguments().get(0).toString());
  }

  @Test
  void shouldGenerateMultipleStringArguments() {
    SourceChange change =
        generator.generate(ctx("addClassName", List.of("class1", "class2", "class3")));

    assertEquals("addClassName", change.getMethodName());
    assertEquals(3, change.getArguments().size());
    assertEquals("\"class1\"", change.getArguments().get(0).toString());
    assertEquals("\"class2\"", change.getArguments().get(1).toString());
    assertEquals("\"class3\"", change.getArguments().get(2).toString());
  }

  @Test
  void shouldGenerateIntegerArguments() {
    SourceChange change = generator.generate(ctx("setValues", List.of(1, 2, 3)));

    assertEquals(3, change.getArguments().size());
    assertTrue(change.getArguments().get(0) instanceof IntegerLiteralExpr);
    assertEquals("1", change.getArguments().get(0).toString());
    assertEquals("2", change.getArguments().get(1).toString());
    assertEquals("3", change.getArguments().get(2).toString());
  }

  @Test
  void shouldGenerateBooleanArguments() {
    SourceChange change = generator.generate(ctx("setFlags", List.of(true, false)));

    assertEquals(2, change.getArguments().size());
    assertTrue(change.getArguments().get(0) instanceof BooleanLiteralExpr);
    assertEquals("true", change.getArguments().get(0).toString());
    assertEquals("false", change.getArguments().get(1).toString());
  }

  @Test
  void shouldReturnFirstArgumentViaGetArgument() {
    SourceChange change = generator.generate(ctx("addClassName", List.of("first", "second")));

    assertEquals("\"first\"", change.getArgument().toString());
  }

  @Test
  void shouldThrowForNonListValue() {
    assertThrows(SourceModificationException.class,
        () -> generator.generate(ctx("addClassName", "not a list")));
  }

  @Test
  void shouldReturnNullForEmptyList() {
    SourceChange change = generator.generate(ctx("addClassName", Collections.emptyList()));
    assertNull(change);
  }

  @Test
  void shouldGenerateStringLiteralExpressions() {
    SourceChange change = generator.generate(ctx("addClassName", List.of("test")));

    assertEquals(StringLiteralExpr.class, change.getArguments().get(0).getClass());
  }
}
