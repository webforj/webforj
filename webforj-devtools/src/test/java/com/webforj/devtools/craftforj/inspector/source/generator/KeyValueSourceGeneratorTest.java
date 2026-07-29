package com.webforj.devtools.craftforj.inspector.source.generator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator.GeneratorContext;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class KeyValueSourceGeneratorTest {

  private final SourceGenerator generator = new KeyValueSourceGenerator();

  private GeneratorContext ctx(String methodName, Object value) {
    FeatureProperty prop = FeatureProperty.builder("Style", "HasStyle").text().value(value).build();
    return new GeneratorContext(methodName, prop);
  }

  @Test
  void shouldGenerateFromListWithTwoElements() {
    SourceChange change = generator.generate(ctx("setStyle", List.of("flex-grow", "1")));

    assertEquals("setStyle", change.getMethodName());
    assertEquals(2, change.getArguments().size());
    assertEquals("\"flex-grow\"", change.getArguments().get(0).toString());
    assertEquals("\"1\"", change.getArguments().get(1).toString());
  }

  @Test
  void shouldGenerateFromMapWithKeyAndValue() {
    SourceChange change =
        generator.generate(ctx("setStyle", Map.of("key", "margin", "value", "10px")));

    assertEquals("setStyle", change.getMethodName());
    assertEquals(2, change.getArguments().size());
    assertEquals("\"margin\"", change.getArguments().get(0).toString());
    assertEquals("\"10px\"", change.getArguments().get(1).toString());
  }

  @Test
  void shouldHandleIntegerValues() {
    SourceChange change = generator.generate(ctx("setValues", List.of("count", 2)));

    assertEquals(2, change.getArguments().size());
    assertEquals("\"count\"", change.getArguments().get(0).toString());
    assertEquals("2", change.getArguments().get(1).toString());
  }

  @Test
  void shouldHandleDoubleValues() {
    SourceChange change = generator.generate(ctx("setValues", List.of("opacity", 0.5)));

    assertEquals(2, change.getArguments().size());
    assertEquals("\"opacity\"", change.getArguments().get(0).toString());
    assertEquals("0.5", change.getArguments().get(1).toString());
  }

  @Test
  void shouldReturnNullForNullValue() {
    SourceChange change = generator.generate(ctx("setStyle", null));
    assertNull(change);
  }

  @Test
  void shouldReturnNullForEmptyStringValue() {
    SourceChange change = generator.generate(ctx("setStyle", List.of("flex-grow", "")));
    assertNull(change);
  }

  @Test
  void shouldReturnNullForNullSecondElement() {
    SourceChange change = generator.generate(ctx("setStyle", Arrays.asList("flex-grow", null)));
    assertNull(change);
  }

  @Test
  void shouldReturnNullForListWithLessThanTwoElements() {
    SourceChange change = generator.generate(ctx("setStyle", List.of("flex-grow")));
    assertNull(change);
  }

  @Test
  void shouldThrowForMissingKey() {
    assertThrows(SourceModificationException.class,
        () -> generator.generate(ctx("setStyle", Arrays.asList(null, "value"))));
  }

  @Test
  void shouldThrowForNonListNonMapValue() {
    assertThrows(SourceModificationException.class,
        () -> generator.generate(ctx("setStyle", "not a list or map")));
  }

  @Test
  void shouldSetMatchKeyFromList() {
    SourceChange change = generator.generate(ctx("setStyle", List.of("flex-grow", "1")));

    assertEquals("flex-grow", change.getMatchKey());
  }

  @Test
  void shouldSetMatchKeyFromMap() {
    SourceChange change =
        generator.generate(ctx("setStyle", Map.of("key", "margin", "value", "10px")));

    assertEquals("margin", change.getMatchKey());
  }
}
