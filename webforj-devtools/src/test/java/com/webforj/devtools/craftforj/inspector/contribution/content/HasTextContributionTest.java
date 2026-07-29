package com.webforj.devtools.craftforj.inspector.contribution.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasText;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasTextContributionTest {

  private final HasTextContribution contribution = new HasTextContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getText()).thenReturn("Hello");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Text", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Hello", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "New Text"));
    verify(component).setText("New Text");
  }

  abstract static class TestComponent extends Component implements HasText<Component> {
  }
}
