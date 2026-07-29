package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasMaxWidth;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMaxWidthContributionTest {

  private final HasMaxWidthContribution contribution = new HasMaxWidthContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMaxWidth()).thenReturn("500px");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MaxWidth", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("500px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "600px"));
    verify(component).setMaxWidth("600px");
  }

  abstract static class TestComponent extends Component implements HasMaxWidth<Component> {
  }
}
