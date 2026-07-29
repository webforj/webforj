package com.webforj.devtools.craftforj.inspector.contribution.validation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasMaxLength;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMaxLengthContributionTest {

  private final HasMaxLengthContribution contribution = new HasMaxLengthContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getMaxLength()).thenReturn(100);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MaxLength", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(100, result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, 200));
    verify(component).setMaxLength(200);
  }

  abstract static class TestComponent extends Component implements HasMaxLength<Component> {
  }
}
