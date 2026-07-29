package com.webforj.devtools.craftforj.inspector.contribution.layout.drawer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.drawer.Drawer;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class DrawerPlacementContributionTest {

  private final DrawerPlacementContribution contribution = new DrawerPlacementContribution();

  @Test
  void shouldGet() {
    Drawer drawer = mock(Drawer.class);
    when(drawer.getPlacement()).thenReturn(Drawer.Placement.RIGHT);

    var result = contribution.get(drawer);

    assertTrue(result.isPresent());
    assertEquals("Placement", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.drawer.Drawer.Placement.RIGHT", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Drawer drawer = mock(Drawer.class);

    boolean success = contribution.set(drawer, Drawer.Placement.BOTTOM);

    assertTrue(success);
    verify(drawer).setPlacement(Drawer.Placement.BOTTOM);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Drawer drawer = mock(Drawer.class);
    assertEquals(Drawer.Placement.class, contribution.findEnumClass(drawer));
  }
}
