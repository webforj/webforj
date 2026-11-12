package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.data.transformation.TransformationException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class LocalDateTimeZonedDateTimeTransformerTest {

  private final LocalDateTimeZonedDateTimeTransformer transformer =
      new LocalDateTimeZonedDateTimeTransformer().setZone(ZoneId.of("Europe/Berlin"));

  @Test
  void shouldTransformToModelInZone() {
    LocalDateTime viewValue = LocalDateTime.of(2026, 11, 9, 14, 30);

    assertEquals(ZonedDateTime.of(viewValue, ZoneId.of("Europe/Berlin")),
        transformer.transformToModel(viewValue));
  }

  @Test
  void shouldTransformToComponentInZone() {
    ZonedDateTime modelValue = ZonedDateTime.parse("2026-11-09T13:30:00Z[UTC]");

    assertEquals(LocalDateTime.of(2026, 11, 9, 14, 30),
        transformer.transformToComponent(modelValue));
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
    LocalDateTimeZonedDateTimeTransformer systemTransformer =
        new LocalDateTimeZonedDateTimeTransformer();
    LocalDateTime viewValue = LocalDateTime.of(2026, 11, 9, 14, 30);

    assertEquals(viewValue.atZone(ZoneId.systemDefault()),
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
  void shouldRejectValueOutOfRange(ZonedDateTime modelValue) {
    TransformationException exception = assertThrows(TransformationException.class,
        () -> transformer.transformToComponent(modelValue));
    assertEquals("The value is outside the supported date and time range.", exception.getMessage());
  }

  private static Stream<ZonedDateTime> getValuesOutOfRange() {
    return Stream.of(ZonedDateTime.of(LocalDateTime.MAX, ZoneOffset.MIN),
        ZonedDateTime.of(LocalDateTime.MIN, ZoneOffset.MAX));
  }
}
