package com.webforj.devtools.craftforj.inspector.contribution.validation.textarea;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TextAreaParagraphLengthLimitContributionTest {

  private final TextAreaParagraphLengthLimitContribution contribution =
      new TextAreaParagraphLengthLimitContribution();

  @Test
  void shouldGet() {
    TextArea component = mock(TextArea.class);
    when(component.getParagraphLengthLimit()).thenReturn(500);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("ParagraphLengthLimit", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(500, result.get().getValue());
  }

  @Test
  void shouldSet() {
    TextArea component = mock(TextArea.class);

    assertTrue(contribution.set(component, 250));
    verify(component).setParagraphLengthLimit(250);
  }
}
