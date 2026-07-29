package com.webforj.devtools.craftforj.inspector.contribution.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasTooltip;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasTooltipContributionTest {

  private final HasTooltipContribution contribution = new HasTooltipContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getTooltipText()).thenReturn("Tooltip");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("TooltipText", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Tooltip", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "New Tooltip"));
    verify(component).setTooltipText("New Tooltip");
  }

  abstract static class TestComponent extends Component implements HasTooltip<Component> {
  }
}
