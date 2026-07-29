package com.webforj.devtools.craftforj.inspector.contribution.state.applayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AppLayoutDrawerOpenedContributionTest {

  private final AppLayoutDrawerOpenedContribution contribution =
      new AppLayoutDrawerOpenedContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    AppLayout component = mock(AppLayout.class);
    when(component.isDrawerOpened()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("DrawerOpened", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    AppLayout component = mock(AppLayout.class);

    assertTrue(contribution.set(component, value));
    verify(component).setDrawerOpened(value);
  }
}
