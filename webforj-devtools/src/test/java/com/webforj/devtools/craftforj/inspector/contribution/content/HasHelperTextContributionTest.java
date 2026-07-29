package com.webforj.devtools.craftforj.inspector.contribution.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasHelperText;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasHelperTextContributionTest {

  private final HasHelperTextContribution contribution = new HasHelperTextContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getHelperText()).thenReturn("Helper");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("HelperText", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Helper", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "New Helper"));
    verify(component).setHelperText("New Helper");
  }

  abstract static class TestComponent extends Component implements HasHelperText<Component> {
  }
}
