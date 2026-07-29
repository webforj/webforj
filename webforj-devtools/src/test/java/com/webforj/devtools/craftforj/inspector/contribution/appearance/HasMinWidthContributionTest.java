package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasMinWidth;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMinWidthContributionTest {

  private final HasMinWidthContribution contribution = new HasMinWidthContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMinWidth()).thenReturn("50px");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MinWidth", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("50px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "100px"));
    verify(component).setMinWidth("100px");
  }

  abstract static class TestComponent extends Component implements HasMinWidth<Component> {
  }
}
