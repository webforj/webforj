package com.webforj.devtools.craftforj.inspector.contribution.validation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasMinLength;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMinLengthContributionTest {

  private final HasMinLengthContribution contribution = new HasMinLengthContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMinLength()).thenReturn(5);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MinLength", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(5, result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, 10));
    verify(component).setMinLength(10);
  }

  abstract static class TestComponent extends Component implements HasMinLength<Component> {
  }
}
