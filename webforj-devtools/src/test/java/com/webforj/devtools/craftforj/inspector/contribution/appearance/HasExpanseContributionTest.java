package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.ExpanseBase;
import com.webforj.concern.HasExpanse;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasExpanseContributionTest {

  private final HasExpanseContribution contribution = new HasExpanseContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getExpanse()).thenReturn(TestExpanse.LARGE);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Expanse", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(TestExpanse.class.getCanonicalName() + ".LARGE", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    // Set with fully qualified enum value
    assertTrue(contribution.set(component, TestExpanse.class.getCanonicalName() + ".SMALL"));
    verify(component).setExpanse(TestExpanse.SMALL);
  }

  enum TestExpanse implements ExpanseBase {
    SMALL, MEDIUM, LARGE
  }

  abstract static class TestComponent extends Component
      implements HasExpanse<Component, TestExpanse> {
  }
}
