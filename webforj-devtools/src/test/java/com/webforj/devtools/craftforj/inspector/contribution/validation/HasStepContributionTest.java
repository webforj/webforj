package com.webforj.devtools.craftforj.inspector.contribution.validation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasStep;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasStepContributionTest {

  private final HasStepContribution contribution = new HasStepContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getStep()).thenReturn(5);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Step", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("5", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getStep()).thenReturn(0);

    assertTrue(contribution.set(component, "10"));
    verify(component).setStep(10);
  }

  abstract static class TestComponent extends Component implements HasStep<Component, Integer> {
  }
}
