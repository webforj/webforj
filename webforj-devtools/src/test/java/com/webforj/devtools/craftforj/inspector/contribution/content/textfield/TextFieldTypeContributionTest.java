package com.webforj.devtools.craftforj.inspector.contribution.content.textfield;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.field.TextField;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TextFieldTypeContributionTest {

  private final TextFieldTypeContribution contribution = new TextFieldTypeContribution();

  @Test
  void shouldGet() {
    TextField component = mock(TextField.class);
    when(component.getType()).thenReturn(TextField.Type.EMAIL);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Type", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.field.TextField.Type.EMAIL", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TextField component = mock(TextField.class);

    assertTrue(contribution.set(component, TextField.Type.SEARCH));
    verify(component).setType(TextField.Type.SEARCH);
  }

  @Test
  void shouldSetEmail() {
    TextField component = mock(TextField.class);

    assertTrue(contribution.set(component, TextField.Type.EMAIL));
    verify(component).setType(TextField.Type.EMAIL);
  }
}
