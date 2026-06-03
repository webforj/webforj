package com.webforj.component.layout.appnav.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.layout.appnav.AppNav;
import com.webforj.component.layout.appnav.AppNavItem;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class AppNavPinEventTest {

  @Test
  void shouldReturnKeyAndState() {
    AppNavPinEvent event = new AppNavPinEvent(new AppNav(),
        Map.of("key", "/orders", "pinned", true, "keys", List.of("/orders")));

    assertEquals("/orders", event.getKey());
    assertTrue(event.isPinned());
  }

  @Test
  void shouldReturnKeys() {
    AppNavPinEvent event = new AppNavPinEvent(new AppNav(),
        Map.of("key", "/orders", "pinned", true, "keys", List.of("/orders", "/users")));

    assertEquals(List.of("/orders", "/users"), event.getKeys());
  }

  @Test
  void shouldResolveItemFromKeyIncludingNested() {
    AppNav nav = new AppNav();
    AppNavItem orders = new AppNavItem("Orders", "/orders");
    AppNavItem group = new AppNavItem("Group");
    AppNavItem nested = new AppNavItem("Nested", "/group/nested");
    group.addItem(nested);
    nav.addItem(orders);
    nav.addItem(group);

    AppNavPinEvent event = new AppNavPinEvent(nav,
        Map.of("key", "/group/nested", "pinned", true, "keys", List.of("/group/nested")));

    assertSame(nested, event.getItem());
  }

  @Test
  void shouldReturnNullWhenItemNotFound() {
    AppNavPinEvent event = new AppNavPinEvent(new AppNav(),
        Map.of("key", "/missing", "pinned", false, "keys", List.of()));

    assertNull(event.getItem());
  }

  @Test
  void shouldReturnNullWhenKeyMissing() {
    AppNav nav = new AppNav();
    nav.addItem(new AppNavItem("Orders", "/orders"));
    AppNavPinEvent event = new AppNavPinEvent(nav, Map.of("pinned", false, "keys", List.of()));

    assertNull(event.getItem());
  }
}
