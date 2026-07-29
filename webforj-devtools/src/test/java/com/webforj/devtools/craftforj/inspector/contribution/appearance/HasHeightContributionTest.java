package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasHeight;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasHeightContributionTest {

  private final HasHeightContribution contribution = new HasHeightContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getHeight()).thenReturn("100px");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Height", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("100px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, "200px"));
    verify(component).setHeight("200px");
  }

  abstract static class TestComponent extends Component implements HasHeight<Component> {
  }
}
