package com.webforj.devtools.craftforj.inspector.contribution.state.radiobutton;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.optioninput.RadioButton;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class RadioButtonActivationContributionTest {

  private final RadioButtonActivationContribution contribution =
      new RadioButtonActivationContribution();

  @Test
  void shouldGet() {
    RadioButton component = mock(RadioButton.class);
    when(component.getActivation()).thenReturn(RadioButton.Activation.AUTO);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Activation", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.optioninput.RadioButton.Activation.AUTO",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    RadioButton component = mock(RadioButton.class);

    assertTrue(contribution.set(component, RadioButton.Activation.MANUAL));
    verify(component).setActivation(RadioButton.Activation.MANUAL);
  }

  @Test
  void shouldSetAuto() {
    RadioButton component = mock(RadioButton.class);

    assertTrue(contribution.set(component, RadioButton.Activation.AUTO));
    verify(component).setActivation(RadioButton.Activation.AUTO);
  }
}
