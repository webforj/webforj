package com.webforj.devtools.craftforj.inspector.contribution.validation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasPattern;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasPatternContributionTest {

  private final HasPatternContribution contribution = new HasPatternContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getPattern()).thenReturn("[a-z]+");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Pattern", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("[a-z]+", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "[0-9]+"));
    verify(component).setPattern("[0-9]+");
  }

  abstract static class TestComponent extends Component implements HasPattern<Component> {
  }
}
