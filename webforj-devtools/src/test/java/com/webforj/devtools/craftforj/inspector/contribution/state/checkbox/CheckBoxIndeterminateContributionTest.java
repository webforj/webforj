package com.webforj.devtools.craftforj.inspector.contribution.state.checkbox;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.optioninput.CheckBox;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class CheckBoxIndeterminateContributionTest {

  private final CheckBoxIndeterminateContribution contribution =
      new CheckBoxIndeterminateContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    CheckBox component = mock(CheckBox.class);
    when(component.isIndeterminate()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Indeterminate", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    CheckBox component = mock(CheckBox.class);

    assertTrue(contribution.set(component, value));
    verify(component).setIndeterminate(value);
  }
}
