package com.webforj.devtools.craftforj.inspector.contribution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.webforj.devtools.craftforj.inspector.contribution.utilities.NumberConverter;
import org.junit.jupiter.api.Test;

class NumberConverterTest {

  @Test
  void shouldReturnNullForNullValue() {
    assertNull(NumberConverter.convert(null, 10));
  }

  @Test
  void shouldConvertToIntegerWhenCurrentValueIsInteger() {
    assertEquals(42, NumberConverter.convert("42", 10));
  }

  @Test
  void shouldConvertToDoubleWhenCurrentValueIsDouble() {
    assertEquals(3.14, NumberConverter.convert("3.14", 1.0));
  }

  @Test
  void shouldInferDoubleFromDecimalPoint() {
    assertEquals(2.5, NumberConverter.convert("2.5", null));
  }

  @Test
  void shouldInferIntegerFromWholeNumber() {
    assertEquals(100, NumberConverter.convert("100", null));
  }

  @Test
  void shouldReturnCurrentValueOnInvalidInteger() {
    Integer current = 50;
    assertEquals(current, NumberConverter.convert("not-a-number", current));
  }

  @Test
  void shouldReturnCurrentValueOnInvalidDouble() {
    Double current = 1.5;
    assertEquals(current, NumberConverter.convert("invalid", current));
  }

  @Test
  void shouldHandleNumericInputDirectly() {
    assertEquals(25, NumberConverter.convert(25, 10));
    assertEquals(3.14, NumberConverter.convert(3.14, 1.0));
  }
}
