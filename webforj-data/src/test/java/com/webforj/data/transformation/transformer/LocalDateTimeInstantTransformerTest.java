package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.data.transformation.TransformationException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class LocalDateTimeInstantTransformerTest {

  private final LocalDateTimeInstantTransformer transformer =
      new LocalDateTimeInstantTransformer().setZone(ZoneId.of("Europe/Berlin"));

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToModel(LocalDateTime viewValue, Instant modelValue) {
    assertEquals(modelValue, transformer.transformToModel(viewValue));
  }

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToComponent(LocalDateTime viewValue, Instant modelValue) {
    assertEquals(viewValue, transformer.transformToComponent(modelValue));
  }

  @Test
  void shouldKeepNullInBothDirections() {
    assertNull(transformer.transformToModel(null));
    assertNull(transformer.transformToComponent(null));
  }

  @Test
  void shouldReturnZone() {
    assertEquals(ZoneId.of("Europe/Berlin"), transformer.getZone());
  }

  @Test
  void shouldUseSystemDefaultZoneWithoutArguments() {
    LocalDateTimeInstantTransformer systemTransformer = new LocalDateTimeInstantTransformer();
    LocalDateTime viewValue = LocalDateTime.of(2026, 11, 9, 14, 30);

    assertEquals(viewValue.atZone(ZoneId.systemDefault()).toInstant(),
        systemTransformer.transformToModel(viewValue));
  }

  @Test
  void shouldRejectNullZone() {
    ZoneId zone = null;
    NullPointerException exception =
        assertThrows(NullPointerException.class, () -> transformer.setZone(zone));
    assertEquals("zone cannot be null", exception.getMessage());
  }

  @ParameterizedTest
  @MethodSource("getValuesOutOfRange")
  void shouldRejectInstantOutOfRange(Instant modelValue) {
    TransformationException exception = assertThrows(TransformationException.class,
        () -> transformer.transformToComponent(modelValue));
    assertEquals("The value is outside the supported date and time range.", exception.getMessage());
  }

  private static Stream<Arguments> getValues() {
    return Stream.of(
        Arguments.of(LocalDateTime.of(2026, 11, 9, 14, 30), Instant.parse("2026-11-09T13:30:00Z")),
        Arguments.of(LocalDateTime.of(2026, 7, 1, 0, 5), Instant.parse("2026-06-30T22:05:00Z")),
        Arguments.of(LocalDateTime.of(1970, 1, 1, 1, 0), Instant.EPOCH));
  }

  private static Stream<Instant> getValuesOutOfRange() {
    return Stream.of(Instant.MAX, Instant.MIN);
  }
}
