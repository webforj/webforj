package com.webforj.devtools.craftforj.inspector.contribution.state.tabbedpane;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TabbedPaneSwipeWithMouseContributionTest {

  private final TabbedPaneSwipeWithMouseContribution contribution =
      new TabbedPaneSwipeWithMouseContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    TabbedPane component = mock(TabbedPane.class);
    when(component.isSwipeWithMouse()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SwipeWithMouse", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    TabbedPane component = mock(TabbedPane.class);

    assertTrue(contribution.set(component, value));
    verify(component).setSwipeWithMouse(value);
  }
}
