package com.webforj.component.layout.applayout;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.icons.Icon;
import org.junit.jupiter.api.Test;

class AppDrawerToggleTest {

  @Test
  void shouldCreateDrawerToggleWithIconNameAndPool() {
    AppDrawerToggle toggle = new AppDrawerToggle("custom-icon", "custom-pool");

    assertEquals("custom-icon", toggle.getName());
    assertEquals("custom-pool", toggle.getPool());
    assertEquals("", toggle.getAttribute("data-drawer-toggle"));
  }

  @Test
  void shouldCreateDrawerToggleWithIcon() {
    Icon icon = new Icon("icon-name", "icon-pool");
    AppDrawerToggle toggle = new AppDrawerToggle(icon);

    assertEquals("icon-name", toggle.getName());
    assertEquals("icon-pool", toggle.getPool());
    assertEquals("", toggle.getAttribute("data-drawer-toggle"));
  }

  @Test
  void shouldCreateDrawerToggleWithDefaultIcon() {
    AppDrawerToggle toggle = new AppDrawerToggle();

    assertEquals("menu-2", toggle.getName());
    assertEquals("tabler", toggle.getPool());
    assertEquals("", toggle.getAttribute("data-drawer-toggle"));
  }
}
