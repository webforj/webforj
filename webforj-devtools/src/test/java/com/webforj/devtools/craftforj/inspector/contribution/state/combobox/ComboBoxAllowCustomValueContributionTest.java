package com.webforj.devtools.craftforj.inspector.contribution.state.combobox;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.list.ComboBox;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ComboBoxAllowCustomValueContributionTest {

  private final ComboBoxAllowCustomValueContribution contribution =
      new ComboBoxAllowCustomValueContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    ComboBox component = mock(ComboBox.class);
    when(component.isAllowCustomValue()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("AllowCustomValue", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    ComboBox component = mock(ComboBox.class);

    assertTrue(contribution.set(component, value));
    verify(component).setAllowCustomValue(value);
  }
}
