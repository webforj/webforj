package com.webforj.devtools.craftforj.inspector.contribution.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasPlaceholder;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasPlaceholderContributionTest {

  private final HasPlaceholderContribution contribution = new HasPlaceholderContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getPlaceholder()).thenReturn("Placeholder");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Placeholder", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Placeholder", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "New Placeholder"));
    verify(component).setPlaceholder("New Placeholder");
  }

  abstract static class TestComponent extends Component implements HasPlaceholder<Component> {
  }
}
