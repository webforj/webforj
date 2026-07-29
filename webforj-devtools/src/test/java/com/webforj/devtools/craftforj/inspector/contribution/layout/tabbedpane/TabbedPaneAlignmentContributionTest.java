package com.webforj.devtools.craftforj.inspector.contribution.layout.tabbedpane;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TabbedPaneAlignmentContributionTest {

  private final TabbedPaneAlignmentContribution contribution =
      new TabbedPaneAlignmentContribution();

  @Test
  void shouldGet() {
    TabbedPane component = mock(TabbedPane.class);
    when(component.getAlignment()).thenReturn(TabbedPane.Alignment.CENTER);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Alignment", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(TabbedPane.Alignment.class.getCanonicalName() + ".CENTER",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    TabbedPane component = mock(TabbedPane.class);

    assertTrue(contribution.set(component, TabbedPane.Alignment.class.getCanonicalName() + ".END"));
    verify(component).setAlignment(TabbedPane.Alignment.END);
  }
}
