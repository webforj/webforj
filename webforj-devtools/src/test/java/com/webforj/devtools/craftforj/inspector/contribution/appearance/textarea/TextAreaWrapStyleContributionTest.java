package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TextAreaWrapStyleContributionTest {

  private final TextAreaWrapStyleContribution contribution = new TextAreaWrapStyleContribution();

  @Test
  void shouldGet() {
    TextArea component = mock(TextArea.class);
    when(component.getWrapStyle()).thenReturn(TextArea.WrapStyle.WORD_BOUNDARIES);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("WrapStyle", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.field.TextArea.WrapStyle.WORD_BOUNDARIES",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    TextArea component = mock(TextArea.class);

    assertTrue(contribution.set(component, TextArea.WrapStyle.CHARACTER_BOUNDARIES));
    verify(component).setWrapStyle(TextArea.WrapStyle.CHARACTER_BOUNDARIES);
  }
}
