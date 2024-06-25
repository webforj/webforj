package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjInputTSpinner;
import com.basis.startup.type.BBjException;
import com.webforj.component.Expanse;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.time.LocalTime;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedTimeFieldSpinnerTest {
  @Mock
  SpinnableMixin spinnableMixin;

  @InjectMocks
  MaskedTimeFieldSpinner component = new MaskedTimeFieldSpinner();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      LocalTime value = LocalTime.of(12, 30);
      component = new MaskedTimeFieldSpinner("label", value, "placeholder");
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
      component = new MaskedTimeFieldSpinner("label", value, listener);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      LocalTime value = LocalTime.of(12, 30);
      component = new MaskedTimeFieldSpinner("label", value);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<LocalTime>> listener = event -> {
      };
      component = new MaskedTimeFieldSpinner("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<LocalTime>> listener = event -> {
      };
      component = new MaskedTimeFieldSpinner(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new MaskedTimeFieldSpinner("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new MaskedTimeFieldSpinner();
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
    Double step = 2.5d;
    component.setStep(step);
    assertEquals(step, component.getStep());
  }

  @Nested
  class SpinnerFieldApi {

    @Test
    void shouldSetGetSpinnerFieldWhenComponentIsAttached() throws BBjException {
      BBjInputTSpinner control = mock(BBjInputTSpinner.class);
      MaskedTimeFieldSpinner spy = spy(component);
      when(spy.inferTimeSpinnerField()).thenReturn(control);

      MaskedTimeFieldSpinner.SpinField field = MaskedTimeFieldSpinner.SpinField.MINUTE;
      spy.setSpinField(field);
      assertEquals(field, spy.getSpinField());

      verify(control).setSpinField(field.getValue());
    }

    @Test
    void shouldSetGetSpinnerFieldWhenComponentIsNotAttached() throws BBjException {
      BBjInputTSpinner control = mock(BBjInputTSpinner.class);
      MaskedTimeFieldSpinner spy = spy(component);

      MaskedTimeFieldSpinner.SpinField field = MaskedTimeFieldSpinner.SpinField.MINUTE;
      spy.setSpinField(field);
      assertEquals(field, spy.getSpinField());

      verify(control, times(0)).setSpinField(field.getValue());

      when(spy.inferTimeSpinnerField()).thenReturn(control);
      spy.onAttach();

      verify(control).setSpinField(field.getValue());
    }
  }
}
