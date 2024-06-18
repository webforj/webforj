package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjInputNSpinner;
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
  MaskedNumberFieldSpinner component;

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
    Float step = 23.5f;
    component.setStep(step);
    assertEquals(step, component.getStep());
  }
}
