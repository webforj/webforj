package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class NumberFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  NumberField component = new NumberField();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      component = new NumberField("label", 10.0, "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals(10.0, component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new NumberField("label", 10.0, listener);
      assertEquals("label", component.getLabel());
      assertEquals(10.0, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      component = new NumberField("label", 10.0);
      assertEquals("label", component.getLabel());
      assertEquals(10.0, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new NumberField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new NumberField(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new NumberField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new NumberField();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Test
  @DisplayName("max")
  void max() throws BBjException {
    component.setMax(10d);
    assertEquals(10d, component.getMax());
    verify(control, times(1)).setProperty("max", 10d);
  }

  @Test
  @DisplayName("min")
  void min() throws BBjException {
    component.setMin(10d);
    assertEquals(10d, component.getMin());
    verify(control, times(1)).setProperty("min", 10d);
  }

  @Test
  @DisplayName("step")
  void step() throws BBjException {
    component.setStep(10d);
    assertEquals(10d, component.getStep());
    verify(control, times(1)).setProperty("step", 10d);
  }

  @Test
  @DisplayName("step null")
  void stepNull() throws BBjException {
    component.setStep(null);
    assertEquals(null, component.getStep());
    verify(control, times(1)).setProperty("step", "any");
  }

  @Nested
  @DisplayName("value API")
  class ValueApi {

    @Test
    @DisplayName("set/get value when the control is null")
    void setGetValueNullControl() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setValue(10d);
      assertEquals(10d, component.getValue());
    }

    @Test
    @DisplayName("set/get value when the control is not null")
    void setGetValueNotNullControl() throws BBjException {
      doReturn("10").when(control).getText();

      component.setValue(10d);
      assertEquals(10d, component.getValue());
    }

    @Test
    @DisplayName("when the value is null")
    void valueNull() throws BBjException {
      doReturn("").when(control).getText();

      assertEquals(null, component.getValue());
    }

    @Test
    void shouldSetValueAsInt() {
      NumberField spy = spy(component);
      spy.setValue(10.0);
      verify(spy, times(1)).setText("10");
    }
  }

  @Test
  void shouldSetGetPattern() throws BBjException {
    String expectedPattern = "[0-9]{3}";
    component.setPattern(expectedPattern);
    assertEquals(expectedPattern, component.getPattern());

    verify(control, times(1)).setProperty("pattern", expectedPattern);
    verify(control, times(0)).getProperty("pattern");
  }
}
