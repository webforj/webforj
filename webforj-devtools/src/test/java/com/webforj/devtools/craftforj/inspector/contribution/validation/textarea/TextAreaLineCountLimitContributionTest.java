package com.webforj.devtools.craftforj.inspector.contribution.validation.textarea;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TextAreaLineCountLimitContributionTest {

  private final TextAreaLineCountLimitContribution contribution =
      new TextAreaLineCountLimitContribution();

  @Test
  void shouldGet() {
    TextArea component = mock(TextArea.class);
    when(component.getLineCountLimit()).thenReturn(100);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("LineCountLimit", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(100, result.get().getValue());
  }

  @Test
  void shouldSet() {
    TextArea component = mock(TextArea.class);

    assertTrue(contribution.set(component, 50));
    verify(component).setLineCountLimit(50);
  }
}
