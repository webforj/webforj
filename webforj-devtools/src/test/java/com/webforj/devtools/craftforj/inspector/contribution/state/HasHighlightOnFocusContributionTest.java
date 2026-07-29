package com.webforj.devtools.craftforj.inspector.contribution.state;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.concern.HasHighlightOnFocus.Behavior;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasHighlightOnFocusContributionTest {

  private final HasHighlightOnFocusContribution contribution =
      new HasHighlightOnFocusContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getHighlightOnFocus()).thenReturn(Behavior.KEY_MOUSE);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("HighlightOnFocus", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(Behavior.class.getCanonicalName() + ".KEY_MOUSE", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, Behavior.class.getCanonicalName() + ".ALL"));
    verify(component).setHighlightOnFocus(Behavior.ALL);
  }

  abstract static class TestComponent extends Component implements HasHighlightOnFocus<Component> {
  }
}
