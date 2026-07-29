package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TextAreaHorizontalScrollContributionTest {

  private final TextAreaHorizontalScrollContribution contribution =
      new TextAreaHorizontalScrollContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    TextArea component = mock(TextArea.class);
    when(component.isHorizontalScroll()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("HorizontalScroll", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    TextArea component = mock(TextArea.class);

    assertTrue(contribution.set(component, value));
    verify(component).setHorizontalScroll(value);
  }
}
