package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjInputDSpinner;
import com.basis.startup.type.BBjException;
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
  MaskedDateFieldSpinner component;

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
