package com.webforj.devtools.craftforj.inspector.contribution.layout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.concern.HasHorizontalAlignment.Alignment;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasHorizontalAlignmentContributionTest {

  private final HasHorizontalAlignmentContribution contribution =
      new HasHorizontalAlignmentContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getHorizontalAlignment()).thenReturn(Alignment.MIDDLE);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("HorizontalAlignment", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(Alignment.class.getCanonicalName() + ".MIDDLE", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, Alignment.class.getCanonicalName() + ".RIGHT"));
    verify(component).setHorizontalAlignment(Alignment.RIGHT);
  }

  abstract static class TestComponent extends Component
      implements HasHorizontalAlignment<Component> {
  }
}
