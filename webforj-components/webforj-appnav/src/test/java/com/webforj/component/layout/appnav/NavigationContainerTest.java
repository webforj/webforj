package com.webforj.component.layout.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mockStatic;

import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.history.MemoryHistory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class NavigationContainerTest {

  MockNavigationContainer container;
  MockedStatic<Router> mockedRouter;

  @BeforeEach
  void setUp() {
    container = new MockNavigationContainer("test-slot");
    Router router = new Router(new RouteRegistry(), new MemoryHistory());
    mockedRouter = mockStatic(Router.class);
    mockedRouter.when(Router::getCurrent).thenReturn(router);
  }

  @AfterEach
  void tearDown() {
    mockedRouter.close();
  }

  @Nested
  class AddItemTests {

    @Test
    void shouldAddItem() {
      AppNavItem item = new AppNavItem("Home", "/home");

      container.addItem(item);
      assertEquals(item, container.getElement().getFirstComponentInSlot("test-slot"));
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
