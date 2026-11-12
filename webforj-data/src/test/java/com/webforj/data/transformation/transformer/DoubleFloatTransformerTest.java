package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.data.transformation.TransformationException;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class DoubleFloatTransformerTest {

  private final DoubleFloatTransformer transformer = new DoubleFloatTransformer();

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToModel(Double viewValue, Float modelValue) {
    assertEquals(modelValue, transformer.transformToModel(viewValue));
  }

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToComponent(Double viewValue, Float modelValue) {
    assertEquals(viewValue, transformer.transformToComponent(modelValue));
  }

  @Test
  void shouldKeepNullInBothDirections() {
    assertNull(transformer.transformToModel(null));
    assertNull(transformer.transformToComponent(null));
  }

  @ParameterizedTest
  @ValueSource(doubles = {3.5E38, -3.5E38, Double.POSITIVE_INFINITY})
  void shouldRejectValueOutOfRange(double viewValue) {
    Double value = viewValue;
    TransformationException exception =
        assertThrows(TransformationException.class, () -> transformer.transformToModel(value));
    assertEquals("The value must be between -3.4028235E38 and 3.4028235E38.",
        exception.getMessage());
  }

  @Test
  void shouldRejectValueThatIsNotNumber() {
    Double viewValue = Double.NaN;
    TransformationException exception =
        assertThrows(TransformationException.class, () -> transformer.transformToModel(viewValue));
    assertEquals("The value must be a finite number.", exception.getMessage());
  }

  private static Stream<Arguments> getValues() {
    return Stream.of(Arguments.of(0.0, 0.0f), Arguments.of(1.1, 1.1f),
        Arguments.of(-12.75, -12.75f), Arguments.of(3.4028235E38, Float.MAX_VALUE));
  }
}
