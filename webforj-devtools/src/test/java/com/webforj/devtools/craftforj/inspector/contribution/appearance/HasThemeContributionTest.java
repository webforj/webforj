package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.ThemeBase;
import com.webforj.concern.HasTheme;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasThemeContributionTest {

  private final HasThemeContribution contribution = new HasThemeContribution();

  @Test
  void shouldGet() {
    TestComponent component = mock(TestComponent.class);
    when(component.getTheme()).thenReturn(TestTheme.PRIMARY);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Theme", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(TestTheme.class.getCanonicalName() + ".PRIMARY", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TestComponent component = mock(TestComponent.class);

    // Set with fully qualified enum value
    assertTrue(contribution.set(component, TestTheme.class.getCanonicalName() + ".SECONDARY"));
    verify(component).setTheme(TestTheme.SECONDARY);
  }

  @Test
  void shouldFindEnumClassFromTypeHierarchy() {
    TestComponent component = mock(TestComponent.class);

    assertEquals(TestTheme.class, contribution.findEnumClass(component));
  }

  @Test
  void shouldReturnNullWhenNoEnumInHierarchy() {
    Component component = mock(Component.class);

    assertEquals(null, contribution.findEnumClass(component));
  }

  enum TestTheme implements ThemeBase {
    PRIMARY, SECONDARY, DANGER
  }

  abstract static class TestComponent extends Component implements HasTheme<Component, TestTheme> {
  }
}
