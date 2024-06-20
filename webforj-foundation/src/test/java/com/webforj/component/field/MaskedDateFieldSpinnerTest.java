package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjInputDSpinner;
import com.basis.startup.type.BBjException;
import com.webforj.component.Expanse;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.time.LocalDate;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedDateFieldSpinnerTest {
  @Mock
  SpinnableMixin spinnableMixin;

  @InjectMocks
  MaskedDateFieldSpinner component = new MaskedDateFieldSpinner();

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

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      LocalDate value = LocalDate.of(2020, 10, 1);
      component = new MaskedDateFieldSpinner("label", value, "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      LocalDate value = LocalDate.of(2020, 10, 1);
      EventListener<ValueChangeEvent<LocalDate>> listener = event -> {
      };
      component = new MaskedDateFieldSpinner("label", value, listener);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      LocalDate value = LocalDate.of(2020, 10, 1);
      component = new MaskedDateFieldSpinner("label", value);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<LocalDate>> listener = event -> {
      };
      component = new MaskedDateFieldSpinner("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<LocalDate>> listener = event -> {
      };
      component = new MaskedDateFieldSpinner(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new MaskedDateFieldSpinner("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new MaskedDateFieldSpinner();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }


  @Nested
  class SpinnerFieldApi {

    @Test
    void shouldSetGetSpinnerFieldWhenComponentIsAttached() throws BBjException {
      BBjInputDSpinner control = mock(BBjInputDSpinner.class);
      MaskedDateFieldSpinner spy = spy(component);
      when(spy.inferDateSpinnerField()).thenReturn(control);

      MaskedDateFieldSpinner.SpinField field = MaskedDateFieldSpinner.SpinField.DAY;
      spy.setSpinField(field);
      assertEquals(field, spy.getSpinField());

      verify(control).setSpinField(field.getValue());
    }

    @Test
    void shouldSetGetSpinnerFieldWhenComponentIsNotAttached() throws BBjException {
      BBjInputDSpinner control = mock(BBjInputDSpinner.class);
      MaskedDateFieldSpinner spy = spy(component);

      MaskedDateFieldSpinner.SpinField field = MaskedDateFieldSpinner.SpinField.DAY;
      spy.setSpinField(field);
      assertEquals(field, spy.getSpinField());

      verify(control, times(0)).setSpinField(field.getValue());

      when(spy.inferDateSpinnerField()).thenReturn(control);
      spy.onAttach();

      verify(control).setSpinField(field.getValue());
    }
  }

}
