package com.webforj.component.layout.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class NavigationContainerTest {

  MockNavigationContainer container;

  @BeforeEach
  void setUp() {
    container = new MockNavigationContainer("test-slot");
  }

  @Nested
  class AddItemTests {

    @Test
    void shouldAddItem() {
      AppNavItem item = new AppNavItem("Home", "/home");

      container.addItem(item);

      assertEquals("test-slot", item.getAttribute("slot"));
    }

    @Test
    void shouldThrowWhenAddingDestroyedItem() {
      AppNavItem item = new AppNavItem("Home", "/home");
      item.destroy();

      assertThrows(IllegalStateException.class, () -> container.addItem(item));
    }
  }

  @Nested
  class RemoveItemTests {

    @Test
    void shouldRemoveItem() {
      AppNavItem item = new AppNavItem("Home", "/home");

      container.addItem(item);
      container.removeItem(item);

      assertEquals(true, item.isDestroyed());
    }
  }
}
