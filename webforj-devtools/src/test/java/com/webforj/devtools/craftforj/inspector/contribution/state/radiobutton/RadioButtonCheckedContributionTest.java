package com.webforj.devtools.craftforj.inspector.contribution.state.radiobutton;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.optioninput.RadioButton;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class RadioButtonCheckedContributionTest {

  private final RadioButtonCheckedContribution contribution = new RadioButtonCheckedContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    RadioButton component = mock(RadioButton.class);
    when(component.isChecked()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Checked", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    RadioButton component = mock(RadioButton.class);

    assertTrue(contribution.set(component, value));
    verify(component).setChecked(value);
  }
}
