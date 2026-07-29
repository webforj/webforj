package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasMaxHeight;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMaxHeightContributionTest {

  private final HasMaxHeightContribution contribution = new HasMaxHeightContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMaxHeight()).thenReturn("500px");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MaxHeight", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("500px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "600px"));
    verify(component).setMaxHeight("600px");
  }

  abstract static class TestComponent extends Component implements HasMaxHeight<Component> {
  }
}
