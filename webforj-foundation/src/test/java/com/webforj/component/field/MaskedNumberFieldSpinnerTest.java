package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjInputNSpinner;
import com.webforj.component.Expanse;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedNumberFieldSpinnerTest {
  @Mock
  SpinnableMixin spinnableMixin;

  @Mock
  BBjInputNSpinner control;

  @InjectMocks
  MaskedNumberFieldSpinner component = new MaskedNumberFieldSpinner();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      Double value = 23.5d;
      component = new MaskedNumberFieldSpinner("label", value, "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      Double value = 23.5d;
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new MaskedNumberFieldSpinner("label", value, listener);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      Double value = 23.5d;
      component = new MaskedNumberFieldSpinner("label", value);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new MaskedNumberFieldSpinner("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new MaskedNumberFieldSpinner(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new MaskedNumberFieldSpinner("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new MaskedNumberFieldSpinner();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Test
  void shouldSpinUp() {
    component.spinUp();
    verify(spinnableMixin).spinUp();
  }

  @Test
  void shouldSpinDown() {
    component.spinDown();
    verify(spinnableMixin).spinDown();
  }

  @Test
  void shouldSetGetStepValue() {
    Double step = 23.5d;
    component.setStep(step);
    assertEquals(step, component.getStep());
  }
}
