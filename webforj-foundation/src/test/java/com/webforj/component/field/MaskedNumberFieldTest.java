package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.startup.type.BBjException;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedNumberFieldTest {
  @Mock
  BBjInputN control;

  @InjectMocks
  MaskedNumberField component = new MaskedNumberField();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      Float value = 23.5f;
      component = new MaskedNumberField("label", value, "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      Float value = 23.5f;
      EventListener<ValueChangeEvent<Float>> listener = event -> {
      };
      component = new MaskedNumberField("label", value, listener);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      Float value = 23.5f;
      component = new MaskedNumberField("label", value);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<Float>> listener = event -> {
      };
      component = new MaskedNumberField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<Float>> listener = event -> {
      };
      component = new MaskedNumberField(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new MaskedNumberField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new MaskedNumberField();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Test
  void shouldSetGetValue() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setValue(23.5f);
    assertEquals(23.5f, component.getValue());
    assertEquals("23.5", component.getText());
  }

  @Nested
  class SeparatorsApi {

    @Test
    void shouldSetGroupCharacterWhenControlIsNotNull() throws BBjException {
      String groupCharacter = ".";
      component.setGroupCharacter(groupCharacter);
      assertEquals(groupCharacter, component.getGroupCharacter());

      verify(control).setCommaCharacter(groupCharacter);
    }

    @Test
    void shouldSetGroupCharacterWhenControlIstNull() throws IllegalAccessException, BBjException {
      String groupCharacter = ".";
      ReflectionUtils.nullifyControl(component);

      component.setGroupCharacter(groupCharacter);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setCommaCharacter(groupCharacter);
    }

    @Test
    void shouldSetDecimalCharacterWhenControlIsNotNull() throws BBjException {
      String decimalCharacter = ",";
      component.setDecimalCharacter(decimalCharacter);
      assertEquals(decimalCharacter, component.getDecimalCharacter());

      verify(control).setDotCharacter(decimalCharacter);
    }

    @Test
    void shouldSetDecimalCharacterWhenControlIstNull() throws IllegalAccessException, BBjException {
      String decimalCharacter = ",";
      ReflectionUtils.nullifyControl(component);

      component.setDecimalCharacter(decimalCharacter);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setDotCharacter(decimalCharacter);
    }
  }

  @Nested
  class NegateableApi {
    @Test
    void shouldConfigureNegateableWhenControlIsNotNull() throws BBjException {
      component.setNegateable(false);
      assertEquals(false, component.isNegateable());

      verify(control).setNegateable(false);
    }

    @Test
    void shouldConfigureNegateableWhenControlIstNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setNegateable(false);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setNegateable(false);
    }
  }

  @Nested
  class MaxMinApi {

    @Test
    void shouldSetGetMaxValue() throws BBjException {
      Float expectedMax = 23.5f;
      component.setMax(expectedMax);
      assertEquals(expectedMax, component.getMax());

      verify(control, times(1)).setProperty("max", expectedMax);
      verify(control, times(0)).getProperty("max");
    }

    @Test
    void shouldSetGetMinValue() throws BBjException {
      Float expectedMin = 23.5f;
      component.setMin(expectedMin);
      assertEquals(expectedMin, component.getMin());

      verify(control, times(1)).setProperty("min", expectedMin);
      verify(control, times(0)).getProperty("min");
    }
  }
}
