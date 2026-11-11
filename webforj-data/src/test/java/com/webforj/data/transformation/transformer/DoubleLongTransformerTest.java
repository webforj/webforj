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

class DoubleLongTransformerTest {

  private final DoubleLongTransformer transformer = new DoubleLongTransformer();

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToModel(Double viewValue, Long modelValue) {
    assertEquals(modelValue, transformer.transformToModel(viewValue));
  }

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToComponent(Double viewValue, Long modelValue) {
    assertEquals(viewValue, transformer.transformToComponent(modelValue));
  }

  @Test
  void shouldKeepNullInBothDirections() {
    assertNull(transformer.transformToModel(null));
    assertNull(transformer.transformToComponent(null));
  }

  @ParameterizedTest
  @ValueSource(doubles = {1.5, -0.25, 9.223372036854775807E18, -9.223372036854775808E18, -1.0E19,
      Double.NaN, Double.NEGATIVE_INFINITY})
  void shouldRejectValueThatIsNotWholeOrOutOfRange(double viewValue) {
    Double value = viewValue;
    TransformationException exception =
        assertThrows(TransformationException.class, () -> transformer.transformToModel(value));
    assertEquals(
        "The value must be a whole number between -9223372036854775808 and 9223372036854775807.",
        exception.getMessage());
  }

  private static Stream<Arguments> getValues() {
    return Stream.of(Arguments.of(0.0, 0L), Arguments.of(9876543210.0, 9876543210L),
        Arguments.of(-42.0, -42L), Arguments.of(9007199254740992.0, 9007199254740992L),
        Arguments.of(1.2345678901234568E18, 1234567890123456800L));
  }
}
