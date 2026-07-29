package com.webforj.devtools.craftforj.inspector.contribution.appearance.tabbedpane;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TabbedPaneHideActiveIndicatorContributionTest {

  private final TabbedPaneHideActiveIndicatorContribution contribution =
      new TabbedPaneHideActiveIndicatorContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    TabbedPane component = mock(TabbedPane.class);
    when(component.isHideActiveIndicator()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("HideActiveIndicator", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    TabbedPane component = mock(TabbedPane.class);

    assertTrue(contribution.set(component, value));
    verify(component).setHideActiveIndicator(value);
  }
}
