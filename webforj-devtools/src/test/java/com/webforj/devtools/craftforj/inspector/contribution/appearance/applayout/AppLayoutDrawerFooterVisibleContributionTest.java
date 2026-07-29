package com.webforj.devtools.craftforj.inspector.contribution.appearance.applayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AppLayoutDrawerFooterVisibleContributionTest {

  private final AppLayoutDrawerFooterVisibleContribution contribution =
      new AppLayoutDrawerFooterVisibleContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    AppLayout component = mock(AppLayout.class);
    when(component.isDrawerFooterVisible()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("DrawerFooterVisible", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    AppLayout component = mock(AppLayout.class);

    assertTrue(contribution.set(component, value));
    verify(component).setDrawerFooterVisible(value);
  }
}
