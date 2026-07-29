package com.webforj.devtools.craftforj.inspector.contribution.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasLabel;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasLabelContributionTest {

  private final HasLabelContribution contribution = new HasLabelContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getLabel()).thenReturn("Label");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Label", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Label", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "New Label"));
    verify(component).setLabel("New Label");
  }

  abstract static class TestComponent extends Component implements HasLabel<Component> {
  }
}
