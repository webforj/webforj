package com.webforj.devtools.craftforj.inspector.contribution.validation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasMin;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMinContributionTest {

  private final HasMinContribution contribution = new HasMinContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMin()).thenReturn(10);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Min", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("10", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMin()).thenReturn(0);

    assertTrue(contribution.set(component, "20"));
    verify(component).setMin(20);
  }

  abstract static class TestComponent extends Component implements HasMin<Component, Integer> {
  }
}
