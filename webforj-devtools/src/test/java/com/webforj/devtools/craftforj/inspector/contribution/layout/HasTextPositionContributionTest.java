package com.webforj.devtools.craftforj.inspector.contribution.layout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasTextPosition;
import com.webforj.concern.HasTextPosition.Position;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasTextPositionContributionTest {

  private final HasTextPositionContribution contribution = new HasTextPositionContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getTextPosition()).thenReturn(Position.LEFT);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("TextPosition", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(Position.class.getCanonicalName() + ".LEFT", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, Position.class.getCanonicalName() + ".RIGHT"));
    verify(component).setTextPosition(Position.RIGHT);
  }

  abstract static class TestComponent extends Component implements HasTextPosition<Component> {
  }
}
