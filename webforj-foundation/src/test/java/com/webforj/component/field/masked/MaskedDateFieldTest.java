package com.webforj.component.field.masked;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.transformation.transformer.JulianLocaleDateTransformer;
import java.time.LocalDate;
import java.util.Locale;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedDateFieldTest {
  @Mock
  BBjInputD control;

  @InjectMocks
  MaskedDateField component;

  @Test
  void shouldSetGetPattern() throws BBjException {
    String expectedPattern = "[0-9]{3}";
    component.setPattern(expectedPattern);
    assertEquals(expectedPattern, component.getPattern());

    verify(control, times(1)).setProperty("pattern", expectedPattern);
    verify(control, times(0)).getProperty("pattern");
  }

  @Nested
  class TextApi {

    @Test
    void shouldGetTextIfAttached() throws BBjException {
      component = spy(component);
      String expectedText = "123";
      when(control.getText()).thenReturn(expectedText);
      when(component.isAttached()).thenReturn(true);

      component.setText(expectedText);
      assertEquals(expectedText, component.getText());

      verify(control, times(1)).setText(expectedText);
      verify(control, times(1)).getText();
    }

    @Test
    void shouldGetTheValueIfNotAttached() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      Environment env = mock(Environment.class);
      WebforjBBjBridge bridge = mock(WebforjBBjBridge.class);

      try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
        mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
        when(env.getWebforjHelper()).thenReturn(bridge);

        LocalDate value = LocalDate.of(2020, 10, 1);
        String mask = MaskedDateField.DEFAULT_MASK;
        String expected = "2020-10-01";
        int julian = new JulianLocaleDateTransformer().transformToComponent(value);

        when(bridge.maskDateTime(julian, null, mask)).thenReturn(expected);

        component.setValue(value);
        assertEquals(expected, component.getText());
      }
    }
  }

  @Nested
  class ValueApi {

    @Test
    void shouldSetValueIfControlIsNotNull() throws BBjException {
      LocalDate expectedValue = LocalDate.of(2020, 10, 1);
      component.setValue(expectedValue);

      int julian = new JulianLocaleDateTransformer().transformToComponent(expectedValue);
      verify(control, times(1)).setValue(BasisNumber.valueOf(julian));
    }

    @Test
    void shouldSetValueIfControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      LocalDate expectedValue = LocalDate.of(2020, 10, 1);
      component.setValue(expectedValue);

      assertEquals(expectedValue, component.getValue());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      int julian = new JulianLocaleDateTransformer().transformToComponent(expectedValue);
      verify(control, times(1)).setValue(BasisNumber.valueOf(julian));
    }

    @Test
    void shouldGetValueIfControlIsNotNull() throws BBjException {
      LocalDate expectedValue = LocalDate.of(2020, 10, 1);
      int julian = new JulianLocaleDateTransformer().transformToComponent(expectedValue);
      when(control.getValue()).thenReturn(BasisNumber.valueOf(julian));

      assertEquals(expectedValue, component.getValue());
    }

    @Test
    void shouldGetValueIfControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      LocalDate expectedValue = LocalDate.of(2020, 10, 1);
      component.setValue(expectedValue);

      assertEquals(expectedValue, component.getValue());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      int julian = new JulianLocaleDateTransformer().transformToComponent(expectedValue);
      when(control.getValue()).thenReturn(BasisNumber.valueOf(julian));

      assertEquals(expectedValue, component.getValue());
    }

    @Test
    void shouldParseTextIfValueIsNullAndNotAttached() throws IllegalAccessException {
      String expected = "2020-10-01";

      component = spy(component);
      when(component.isAttached()).thenReturn(false);
      when(component.getText()).thenReturn(expected);
      ReflectionUtils.nullifyControl(component);

      Environment env = mock(Environment.class);
      WebforjBBjBridge bridge = mock(WebforjBBjBridge.class);

      try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
        mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
        when(env.getWebforjHelper()).thenReturn(bridge);

        LocalDate expectedValue = LocalDate.of(2020, 10, 1);
        String mask = MaskedDateField.DEFAULT_MASK;
        int julian = new JulianLocaleDateTransformer().transformToComponent(expectedValue);

        when(bridge.parseDate(expected, mask, Locale.getDefault())).thenReturn(julian);

        assertEquals(expectedValue, component.getValue());
      }
    }

    @Test
    void shouldReturnMaskedValue() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      LocalDate expectedValue = LocalDate.of(2020, 10, 1);
      component.setValue(expectedValue);

      Environment env = mock(Environment.class);
      WebforjBBjBridge bridge = mock(WebforjBBjBridge.class);

      try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
        mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
        when(env.getWebforjHelper()).thenReturn(bridge);

        String mask = MaskedDateField.DEFAULT_MASK;
        String expected = "2020-10-01";
        int julian = new JulianLocaleDateTransformer().transformToComponent(expectedValue);

        when(bridge.maskDateTime(julian, null, mask)).thenReturn(expected);

        assertEquals(expected, component.getMaskedValue());
      }
    }
  }

  @Nested
  class CalendarApi {

    @Test
    void shouldSetGetVisibleCalendarIcon() throws BBjException {
      boolean expectedVisibleCalendarIcon = true;
      component.setVisibleCalendarIcon(expectedVisibleCalendarIcon);
      assertEquals(expectedVisibleCalendarIcon, component.isVisibleCalendarIcon());

      verify(control, times(1)).setProperty("visibleCalendarIcon", expectedVisibleCalendarIcon);
      verify(control, times(0)).getProperty("visibleCalendarIcon");
    }

    @Test
    void shouldSetGetToggleCalendarOnEnter() throws BBjException {
      boolean expectedToggleCalendarOnEnter = true;
      component.setToggleCalendarOnEnter(expectedToggleCalendarOnEnter);
      assertEquals(expectedToggleCalendarOnEnter, component.isToggleCalendarOnEnter());

      verify(control, times(1)).setProperty("toggleCalendarOnEnter", expectedToggleCalendarOnEnter);
      verify(control, times(0)).getProperty("toggleCalendarOnEnter");
    }

    @Test
    void shouldQueueOpenCalendarWhenControlIsNull() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.openCalendar();

      verify(control, times(0)).calendar();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).calendar();
    }

    @Test
    void shouldShowCalendarWeeksWhenControlIsNotNull() throws BBjException {
      boolean expectedShowCalendarWeeks = true;
      component.setShowCalendarWeeks(expectedShowCalendarWeeks);

      verify(control, times(1)).setShowWeeks(expectedShowCalendarWeeks);
    }

    @Test
    void shouldShowCalendarWeeksWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setShowCalendarWeeks(true);

      assertEquals(true, component.isShowCalendarWeeks());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setShowWeeks(true);
    }
  }
}
