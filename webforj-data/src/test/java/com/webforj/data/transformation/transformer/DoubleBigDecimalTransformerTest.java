package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.data.transformation.TransformationException;
import java.math.BigDecimal;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class DoubleBigDecimalTransformerTest {

  private final DoubleBigDecimalTransformer transformer = new DoubleBigDecimalTransformer();

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToModel(Double viewValue, BigDecimal modelValue) {
    assertEquals(modelValue, transformer.transformToModel(viewValue));
  }

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToComponent(Double viewValue, BigDecimal modelValue) {
    assertEquals(viewValue, transformer.transformToComponent(modelValue));
  }

  @Test
  void shouldKeepNullInBothDirections() {
    assertNull(transformer.transformToModel(null));
    assertNull(transformer.transformToComponent(null));
  }

  @ParameterizedTest
  @ValueSource(doubles = {Double.NaN, Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY})
  void shouldRejectValueThatIsNotFinite(double viewValue) {
    Double value = viewValue;
    TransformationException exception =
        assertThrows(TransformationException.class, () -> transformer.transformToModel(value));
    assertEquals("The value must be a finite number.", exception.getMessage());
  }

  private static Stream<Arguments> getValues() {
    return Stream.of(Arguments.of(0.0, new BigDecimal("0")),
        Arguments.of(0.1, new BigDecimal("0.1")), Arguments.of(1234.56, new BigDecimal("1234.56")),
        Arguments.of(-99.5, new BigDecimal("-99.5")),
        Arguments.of(1234.5, new BigDecimal("1234.5")),
        Arguments.of(10000000.0, new BigDecimal("10000000")),
        Arguments.of(1.0E21, new BigDecimal("1000000000000000000000")),
        Arguments.of(1.0E-7, new BigDecimal("0.0000001")));
  }
}
