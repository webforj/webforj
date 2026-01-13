package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.data.transformation.transformer.HoursLocalTimeTransformer;
import com.webforj.data.transformation.transformer.JulianLocaleDateTransformer;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Locale;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskDecoratorTest {

  @Mock
  private Environment env;

  @Mock
  private WebforjBBjBridge bridge;

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
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
      when(env.getBridge()).thenReturn(bridge);

      LocalDate input = LocalDate.now();
      String mask = "%Dz-%Mz-%Yl";
      String expected = "12-06-2023";

      int julian = new JulianLocaleDateTransformer().transformToComponent(input);
      when(bridge.maskDateTime(julian, null, mask)).thenReturn(expected);

      String result = MaskDecorator.forDate(input, mask);

      assertEquals(expected, result);
    }
  }

  @Test
  void shouldMaskTime() {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
      when(env.getBridge()).thenReturn(bridge);

      LocalTime input = LocalTime.of(14, 30, 15);
      String mask = "%Hz:%mz:%sz";
      String expected = "14:30:15";

      double hms = new HoursLocalTimeTransformer().transformToComponent(input);
      when(bridge.maskDateTime(0, hms, mask)).thenReturn(expected);

      String result = MaskDecorator.forTime(input, mask);

      assertEquals(expected, result);
    }
  }

  @Test
  void shouldMaskDateTime() {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
      when(env.getBridge()).thenReturn(bridge);

      LocalDateTime input = LocalDateTime.of(2023, 6, 12, 14, 30, 15);
      String mask = "%Dz-%Mz-%Yl %Hz:%mz:%s";
      String expected = "12-06-2023 14:30:15";

      JulianLocaleDateTransformer dateTransformer = new JulianLocaleDateTransformer();
      HoursLocalTimeTransformer timeTransformer = new HoursLocalTimeTransformer();
      int julian = dateTransformer.transformToComponent(input.toLocalDate());
      double hms = timeTransformer.transformToComponent(input.toLocalTime());
      when(bridge.maskDateTime(julian, hms, mask)).thenReturn(expected);

      String result = MaskDecorator.forDateTime(input, mask);

      assertEquals(expected, result);
    }
  }

  @Test
  void shouldParseDate() {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
      when(env.getBridge()).thenReturn(bridge);

      String input = "12";
      String mask = "%Dz-%Mz-%Yz";
      Locale locale = Locale.US;
      LocalDate expected = LocalDate.of(2023, 6, 12);
      int julian = new JulianLocaleDateTransformer().transformToComponent(expected);

      when(bridge.parseDate(input, mask, locale)).thenReturn(julian);

      LocalDate result = MaskDecorator.parseDate(input, mask, locale);

      assertEquals(expected, result);
    }
  }

  @Test
  void shouldParseTime() {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
      when(env.getBridge()).thenReturn(bridge);

      String input = "9pm";
      String mask = "%Hz:%Mz:%S";
      Locale locale = Locale.US;
      LocalTime expected = LocalTime.of(21, 0, 0);
      double hms = new HoursLocalTimeTransformer().transformToComponent(expected);

      when(bridge.parseTime(input, mask, locale)).thenReturn(hms);

      LocalTime result = MaskDecorator.parseTime(input, mask, locale);

      assertEquals(expected, result);
    }
  }
}
