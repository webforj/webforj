package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TextAreaColumnsContributionTest {

  private final TextAreaColumnsContribution contribution = new TextAreaColumnsContribution();

  @Test
  void shouldGet() {
    TextArea component = mock(TextArea.class);
    when(component.getColumns()).thenReturn(40);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Columns", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(40, result.get().getValue());
  }

  @Test
  void shouldSet() {
    TextArea component = mock(TextArea.class);

    assertTrue(contribution.set(component, 60));
    verify(component).setColumns(60);
  }
}
