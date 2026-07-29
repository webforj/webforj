package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.component.Component;
import com.webforj.concern.HasClassName;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import java.util.Map;
import org.junit.jupiter.api.Test;

class HasClassNameContributionTest {

  private final HasClassNameContribution contribution = new HasClassNameContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("ClassNames", result.get().getName());
    assertEquals(PropertyType.LIST, result.get().getEditorType());
  }

  @Test
  void shouldSetAdd() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, Map.of("action", "add", "item", "my-class")));
    verify(component).addClassName("my-class");
  }

  @Test
  void shouldSetRemove() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, Map.of("action", "remove", "item", "my-class")));
    verify(component).removeClassName("my-class");
  }

  abstract static class TestComponent extends Component implements HasClassName<Component> {
  }
}
