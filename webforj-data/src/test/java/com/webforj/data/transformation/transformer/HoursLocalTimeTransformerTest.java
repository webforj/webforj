package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalTime;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class HoursLocalTimeTransformerTest {

  private final HoursLocalTimeTransformer transformer = new HoursLocalTimeTransformer();

  static Stream<Arguments> provideTestData() {
    return Stream.of(Arguments.of(0.0, LocalTime.of(0, 0, 0, 0)),
        Arguments.of(15.75, LocalTime.of(15, 45, 0, 0)),
        Arguments.of(6.501666666666667, LocalTime.of(6, 30, 6, 0)),
        Arguments.of(18.25, LocalTime.of(18, 15, 0, 0)));
  }

  @ParameterizedTest
  @MethodSource("provideTestData")
  void shouldTransformToModel(Double inputDouble, LocalTime expectedTime) {
    LocalTime resultTime = transformer.transformToModel(inputDouble);
    assertEquals(expectedTime, resultTime);
  }

  @ParameterizedTest
  @MethodSource("provideTestData")
  void testTransformToComponent(Double expectedDouble, LocalTime inputTime) {
    Double resultDouble = transformer.transformToComponent(inputTime);
    assertEquals(expectedDouble, resultDouble);
  }
}
