package com.webforj.devtools.craftforj.inspector.contribution.layout.navigator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.navigator.Navigator;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class NavigatorLayoutContributionTest {

  private final NavigatorLayoutContribution contribution = new NavigatorLayoutContribution();

  @Test
  void shouldGet() {
    Navigator component = mock(Navigator.class);
    when(component.getLayout()).thenReturn(Navigator.Layout.PAGES);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Layout", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(Navigator.Layout.class.getCanonicalName() + ".PAGES", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Navigator component = mock(Navigator.class);

    assertTrue(
        contribution.set(component, Navigator.Layout.class.getCanonicalName() + ".QUICK_JUMP"));
    verify(component).setLayout(Navigator.Layout.QUICK_JUMP);
  }
}
