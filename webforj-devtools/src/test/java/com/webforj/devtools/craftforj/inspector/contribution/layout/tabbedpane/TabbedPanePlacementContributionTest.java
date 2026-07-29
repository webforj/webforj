package com.webforj.devtools.craftforj.inspector.contribution.layout.tabbedpane;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TabbedPanePlacementContributionTest {

  private final TabbedPanePlacementContribution contribution =
      new TabbedPanePlacementContribution();

  @Test
  void shouldGet() {
    TabbedPane component = mock(TabbedPane.class);
    when(component.getPlacement()).thenReturn(TabbedPane.Placement.TOP);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Placement", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(TabbedPane.Placement.class.getCanonicalName() + ".TOP", result.get().getValue());
  }

  @Test
  void shouldSet() {
    TabbedPane component = mock(TabbedPane.class);

    assertTrue(
        contribution.set(component, TabbedPane.Placement.class.getCanonicalName() + ".BOTTOM"));
    verify(component).setPlacement(TabbedPane.Placement.BOTTOM);
  }
}
