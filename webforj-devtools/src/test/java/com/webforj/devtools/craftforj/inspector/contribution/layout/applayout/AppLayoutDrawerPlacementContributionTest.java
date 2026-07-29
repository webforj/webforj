package com.webforj.devtools.craftforj.inspector.contribution.layout.applayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AppLayoutDrawerPlacementContributionTest {

  private final AppLayoutDrawerPlacementContribution contribution =
      new AppLayoutDrawerPlacementContribution();

  @Test
  void shouldGet() {
    AppLayout component = mock(AppLayout.class);
    when(component.getDrawerPlacement()).thenReturn(AppLayout.DrawerPlacement.LEFT);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("DrawerPlacement", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(AppLayout.DrawerPlacement.class.getCanonicalName() + ".LEFT",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    AppLayout component = mock(AppLayout.class);

    assertTrue(
        contribution.set(component, AppLayout.DrawerPlacement.class.getCanonicalName() + ".RIGHT"));
    verify(component).setDrawerPlacement(AppLayout.DrawerPlacement.RIGHT);
  }
}
