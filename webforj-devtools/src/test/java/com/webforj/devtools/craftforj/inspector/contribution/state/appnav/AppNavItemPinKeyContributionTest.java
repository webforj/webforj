package com.webforj.devtools.craftforj.inspector.contribution.state.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.appnav.AppNavItem;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AppNavItemPinKeyContributionTest {

  private final AppNavItemPinKeyContribution contribution = new AppNavItemPinKeyContribution();

  @Test
  void shouldGet() {
    AppNavItem component = mock(AppNavItem.class);
    when(component.getPinKey()).thenReturn("dashboard");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("PinKey", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("dashboard", result.get().getValue());
  }

  @Test
  void shouldSet() {
    AppNavItem component = mock(AppNavItem.class);

    assertTrue(contribution.set(component, "settings"));
    verify(component).setPinKey("settings");
  }
}
