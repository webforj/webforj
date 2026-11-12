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

class DoubleIntegerTransformerTest {

  private final DoubleIntegerTransformer transformer = new DoubleIntegerTransformer();

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToModel(Double viewValue, Integer modelValue) {
    assertEquals(modelValue, transformer.transformToModel(viewValue));
  }

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToComponent(Double viewValue, Integer modelValue) {
    assertEquals(viewValue, transformer.transformToComponent(modelValue));
  }

  @Test
  void shouldKeepNullInBothDirections() {
    assertNull(transformer.transformToModel(null));
    assertNull(transformer.transformToComponent(null));
  }

  @ParameterizedTest
  @ValueSource(
      doubles = {1.5, -0.25, 2147483648.0, -2147483649.0, Double.NaN, Double.POSITIVE_INFINITY})
  void shouldRejectValueThatIsNotWholeOrOutOfRange(double viewValue) {
    Double value = viewValue;
    TransformationException exception =
        assertThrows(TransformationException.class, () -> transformer.transformToModel(value));
    assertEquals("The value must be a whole number between -2147483648 and 2147483647.",
        exception.getMessage());
  }

  private static Stream<Arguments> getValues() {
    return Stream.of(Arguments.of(0.0, 0), Arguments.of(424242.0, 424242), Arguments.of(-17.0, -17),
        Arguments.of(2147483647.0, Integer.MAX_VALUE),
        Arguments.of(-2147483648.0, Integer.MIN_VALUE));
  }
}
