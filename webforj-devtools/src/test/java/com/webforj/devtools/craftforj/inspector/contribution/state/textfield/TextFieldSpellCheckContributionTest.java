package com.webforj.devtools.craftforj.inspector.contribution.state.textfield;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.field.TextField;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TextFieldSpellCheckContributionTest {

  private final TextFieldSpellCheckContribution contribution =
      new TextFieldSpellCheckContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    TextField component = mock(TextField.class);
    when(component.isSpellCheck()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SpellCheck", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    TextField component = mock(TextField.class);

    assertTrue(contribution.set(component, value));
    verify(component).setSpellCheck(value);
  }
}
