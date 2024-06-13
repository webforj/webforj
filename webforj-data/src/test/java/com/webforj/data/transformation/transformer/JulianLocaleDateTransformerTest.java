package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.data.transformation.TransformationException;
import java.time.LocalDate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class JulianLocaleDateTransformerTest {

  private JulianLocaleDateTransformer transformer;

  @BeforeEach
  void setUp() {
    transformer = new JulianLocaleDateTransformer();
  }

  @Test
  void shouldTransformToModelWithValidJulianDate() {
    LocalDate expectedDate = LocalDate.of(2005, 3, 4);
    int julianDate = 2453434;
    LocalDate result = transformer.transformToModel(julianDate);
    assertEquals(expectedDate, result);
  }

  @Test
  void shouldReturnNullForNegativeJulianDate() {
    assertNull(transformer.transformToModel(-1));
  }

  @Test
  void shouldReturnCurrentDateForZeroJulianDate() {
    LocalDate expectedDate = LocalDate.now();
    LocalDate result = transformer.transformToModel(0);
    assertEquals(expectedDate, result);
  }

  @Test
  void shouldThrowExceptionForNullJulianDate() {
    assertThrows(TransformationException.class, () -> {
      transformer.transformToModel(null);
    });
  }

  @Test
  void shouldTransformToComponentWithValidLocalDate() {
    LocalDate date = LocalDate.of(2005, 3, 4);
    int expectedJulianDate = 2453434;
    int result = transformer.transformToComponent(date);
    assertEquals(expectedJulianDate, result);
  }

  @Test
  void shouldReturnNegativeOneForNullLocalDate() {
    assertEquals(-1, transformer.transformToComponent(null));
  }
}
