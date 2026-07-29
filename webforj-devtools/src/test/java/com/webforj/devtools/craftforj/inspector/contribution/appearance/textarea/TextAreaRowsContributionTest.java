package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TextAreaRowsContributionTest {

  private final TextAreaRowsContribution contribution = new TextAreaRowsContribution();

  @Test
  void shouldGet() {
    TextArea component = mock(TextArea.class);
    when(component.getRows()).thenReturn(5);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Rows", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(5, result.get().getValue());
  }

  @Test
  void shouldSet() {
    TextArea component = mock(TextArea.class);

    assertTrue(contribution.set(component, 10));
    verify(component).setRows(10);
  }
}
