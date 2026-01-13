package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Locale;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskDecoratorTest {

  @Mock
  private Environment env;

  @Test
  void shouldMaskString() {
    String input = "qw12";
    String mask = "AA-00";
    String expected = "QW-12";

    String result = MaskDecorator.forString(input, mask);

    assertEquals(expected, result);
  }

  @Test
  void shouldMaskNumber() {
    double input = 12345;
    String mask = "##,###,###.00";
    String expected = "    12,345.00";

    String result = MaskDecorator.forNumber(input, mask);

    assertEquals(expected, result);
  }

  @Test
  void shouldMaskDate() {
    LocalDate input = LocalDate.of(2023, 6, 12);
    String mask = "%Dz-%Mz-%Yl";
    String expected = "12-06-2023";

    String result = MaskDecorator.forDate(input, mask);

    assertEquals(expected, result);
  }

  @Test
  void shouldMaskTime() {
    LocalTime input = LocalTime.of(14, 30, 15);
    String mask = "%Hz:%mz:%sz";
    String expected = "14:30:15";

    String result = MaskDecorator.forTime(input, mask);

    assertEquals(expected, result);
  }

  @Test
  void shouldMaskDateTime() {
    LocalDateTime input = LocalDateTime.of(2023, 6, 12, 14, 30, 15);
    String mask = "%Dz-%Mz-%Yl %Hz:%mz:%sz";
    String expected = "12-06-2023 14:30:15";

    String result = MaskDecorator.forDateTime(input, mask);

    assertEquals(expected, result);
  }

  @Test
  void shouldParseDate() {
    String input = "12-06-2023";
    String mask = "%Dz-%Mz-%Yl";
    Locale locale = Locale.US;
    LocalDate expected = LocalDate.of(2023, 6, 12);

    LocalDate result = MaskDecorator.parseDate(input, mask, locale);

    assertEquals(expected, result);
  }

  @Test
  void shouldParseTime() {
    String input = "9pm";
    String mask = "%Hz:%Mz:%S";
    Locale locale = Locale.US;
    LocalTime expected = LocalTime.of(21, 0, 0);

    LocalTime result = MaskDecorator.parseTime(input, mask, locale);

    assertEquals(expected, result);
  }
}
