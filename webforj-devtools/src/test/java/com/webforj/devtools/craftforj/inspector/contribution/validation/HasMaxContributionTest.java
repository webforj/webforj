package com.webforj.devtools.craftforj.inspector.contribution.validation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasMax;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMaxContributionTest {

  private final HasMaxContribution contribution = new HasMaxContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMax()).thenReturn(100);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Max", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("100", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMax()).thenReturn(0);

    assertTrue(contribution.set(component, "200"));
    verify(component).setMax(200);
  }

  abstract static class TestComponent extends Component implements HasMax<Component, Integer> {
  }
}
