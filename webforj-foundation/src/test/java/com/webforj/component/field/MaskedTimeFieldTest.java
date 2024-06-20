package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjInputT;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.transformation.transformer.HoursLocalTimeTransformer;
import com.webforj.dispatcher.EventListener;
import java.time.Duration;
import java.time.LocalTime;
import java.util.Locale;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedTimeFieldTest {
  @Mock
  BBjInputT control;

  @InjectMocks
  MaskedTimeField component = new MaskedTimeField();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      LocalTime value = LocalTime.of(12, 30);
      component = new MaskedTimeField("label", value, "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      LocalTime value = LocalTime.of(12, 30);
      EventListener<ValueChangeEvent<LocalTime>> listener = event -> {
      };
      component = new MaskedTimeField("label", value, listener);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      LocalTime value = LocalTime.of(12, 30);
      component = new MaskedTimeField("label", value);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<LocalTime>> listener = event -> {
      };
      component = new MaskedTimeField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<LocalTime>> listener = event -> {
      };
      component = new MaskedTimeField(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new MaskedTimeField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new MaskedTimeField();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
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

        LocalTime value = LocalTime.of(12, 30);
        String mask = MaskedTimeField.DEFAULT_MASK;
        String expected = "12:30 PM";
        double hours = new HoursLocalTimeTransformer().transformToComponent(value);

        when(bridge.maskDateTime(0, hours, mask)).thenReturn(expected);

        component.setValue(value);
        assertEquals(expected, component.getText());
      }
    }
  }

  @Nested
  class ValueApi {

    @Test
    void shouldSetValueIfControlIsNotNull() throws BBjException {
      LocalTime expectedValue = LocalTime.of(12, 30);
      component.setValue(expectedValue);

      double hours = new HoursLocalTimeTransformer().transformToComponent(expectedValue);
      verify(control, times(1)).setValue(BasisNumber.valueOf(hours));
    }

    @Test
    void shouldSetValueIfControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      LocalTime expectedValue = LocalTime.of(12, 30);
      component.setValue(expectedValue);

      assertEquals(expectedValue, component.getValue());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      double hours = new HoursLocalTimeTransformer().transformToComponent(expectedValue);
      verify(control, times(1)).setValue(BasisNumber.valueOf(hours));
    }

    @Test
    void shouldGetValueIfControlIsNotNull() throws BBjException {
      LocalTime expectedValue = LocalTime.of(12, 30);
      double hours = new HoursLocalTimeTransformer().transformToComponent(expectedValue);
      when(control.getValue()).thenReturn(BasisNumber.valueOf(hours));

      assertEquals(expectedValue, component.getValue());
    }

    @Test
    void shouldGetValueIfControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      LocalTime expectedValue = LocalTime.of(12, 30);
      component.setValue(expectedValue);

      assertEquals(expectedValue, component.getValue());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      double hours = new HoursLocalTimeTransformer().transformToComponent(expectedValue);
      when(control.getValue()).thenReturn(BasisNumber.valueOf(hours));

      assertEquals(expectedValue, component.getValue());
    }

    @Test
    void shouldParseTextIfValueIsNullAndNotAttached() throws IllegalAccessException {
      String expected = "12:30 PM";

      component = spy(component);
      when(component.isAttached()).thenReturn(false);
      when(component.getText()).thenReturn(expected);
      ReflectionUtils.nullifyControl(component);

      Environment env = mock(Environment.class);
      WebforjBBjBridge bridge = mock(WebforjBBjBridge.class);

      try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
        mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
        when(env.getWebforjHelper()).thenReturn(bridge);

        LocalTime expectedValue = LocalTime.of(12, 30);
        String mask = MaskedTimeField.DEFAULT_MASK;
        double hours = new HoursLocalTimeTransformer().transformToComponent(expectedValue);

        when(bridge.parseTime(expected, mask, Locale.getDefault())).thenReturn(hours);

        assertEquals(expectedValue, component.getValue());
      }
    }

    @Test
    void shouldReturnMaskedValue() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      LocalTime expectedValue = LocalTime.of(12, 30);
      component.setValue(expectedValue);

      Environment env = mock(Environment.class);
      WebforjBBjBridge bridge = mock(WebforjBBjBridge.class);

      try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
        mockedEnvironment.when(Environment::getCurrent).thenReturn(env);
        when(env.getWebforjHelper()).thenReturn(bridge);

        String mask = MaskedTimeField.DEFAULT_MASK;
        String expected = "2020-10-01";
        double hours = new HoursLocalTimeTransformer().transformToComponent(expectedValue);

        when(bridge.maskDateTime(0, hours, mask)).thenReturn(expected);

        assertEquals(expected, component.getMaskedValue());
      }
    }
  }

  @Nested
  class MaxMinApi {

    @Test
    void shouldSetGetMax() throws BBjException {
      LocalTime expectedMax = LocalTime.of(12, 30);
      component.setMax(expectedMax);
      assertEquals(expectedMax, component.getMax());

      double hours = new HoursLocalTimeTransformer().transformToComponent(expectedMax);
      verify(control, times(1)).setProperty("max", hours);
      verify(control, times(0)).getProperty("max");
    }

    @Test
    void shouldSetGetMin() throws BBjException {
      LocalTime expectedMin = LocalTime.of(12, 30);
      component.setMin(expectedMin);
      assertEquals(expectedMin, component.getMin());

      double hours = new HoursLocalTimeTransformer().transformToComponent(expectedMin);
      verify(control, times(1)).setProperty("min", hours);
      verify(control, times(0)).getProperty("min");
    }
  }

  @Nested
  class PickerApi {

    @Test
    void shouldSetGetVisiblePickerIcon() throws BBjException {
      boolean expectedVisiblePickerIcon = true;
      component.getPicker().setIconVisible(expectedVisiblePickerIcon);
      assertEquals(expectedVisiblePickerIcon, component.getPicker().isIconVisible());

      verify(control, times(1)).setProperty("pickerIconVisible", expectedVisiblePickerIcon);
      verify(control, times(0)).getProperty("pickerIconVisible");
    }

    @Test
    void shouldSetGetPickerAutoOpen() throws BBjException {
      boolean expectedPickerAutoOpen = true;
      component.getPicker().setAutoOpen(expectedPickerAutoOpen);
      assertEquals(expectedPickerAutoOpen, component.getPicker().isAutoOpen());

      verify(control, times(1)).setProperty("pickerAutoOpen", expectedPickerAutoOpen);
      verify(control, times(0)).getProperty("pickerAutoOpen");
    }

    @Test
    void shouldSetGetPickerType() throws BBjException {
      String expectedPickerType = "time";
      component.getPicker().setType(expectedPickerType);
      assertEquals(expectedPickerType, component.getPicker().getType());

      verify(control, times(1)).setProperty("pickerType", expectedPickerType);
      verify(control, times(0)).getProperty("pickerType");
    }

    @Test
    void shouldSetGetStep() throws BBjException {
      Duration expectedStep = Duration.ofHours(1);
      component.getPicker().setStep(expectedStep);
      assertEquals(expectedStep, component.getPicker().getStep());

      verify(control, times(1)).setProperty("step", expectedStep.toMillis() / 1000);
      verify(control, times(0)).getProperty("step");
    }

    @Test
    void shouldQueueOpenTimerPickerWhenControlIsNull() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.getPicker().show();

      verify(control, times(0)).showTimePicker();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).showTimePicker();
    }
  }
}
