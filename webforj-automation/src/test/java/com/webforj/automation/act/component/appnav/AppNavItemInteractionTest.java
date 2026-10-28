package com.webforj.automation.act.component.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.automation.find.ComponentLocator;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.component.layout.appnav.AppNavItem;
import com.webforj.component.window.Frame;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.history.MemoryHistory;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AppNavItemInteractionTest {

  @Test
  void shouldNavigateOnTheClickOfTheItemWithPath() throws Exception {
    RouteRegistry registry = new RouteRegistry();
    registry.register(MainView.class);
    registry.register(AboutView.class);
    Router router = new Router(registry, new MemoryHistory());
    Environment environment = FrameEnvironment.create(router);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      router.navigate(MainView.class);
      AppNav nav = new AppNav();
      AppNavItem group = new AppNavItem("Pages");
      AppNavItem about = new AppNavItem("About", AboutView.class);
      group.addItem(about);
      nav.addItem(group);
      frame.add(nav);

      UserInteraction.click(group);
      assertTrue(ComponentLocator.of(MainView.class).isPresent());

      UserInteraction.click(about);

      assertTrue(ComponentLocator.of(AboutView.class).isPresent());
      assertFalse(ComponentLocator.of(MainView.class).isPresent());
    }
  }

  @Test
  void shouldPinAndUnpinTheItemBeforeTheNavigationReportsIt() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AppNav nav = new AppNav();
      nav.getPinning().setEnabled(true);
      AppNavItem orders = new AppNavItem("Orders", "/orders");
      AppNavItem invoices = new AppNavItem("Invoices", "/invoices");
      nav.addItem(orders);
      nav.addItem(invoices);
      List<String> events = new ArrayList<>();
      nav.onPin(event -> events
          .add(event.getItem().getText() + " " + event.isPinned() + " " + event.getKeys().size()));
      frame.add(nav);

      UserInteraction.perform(orders, AppNavItemInteraction.pin());
      UserInteraction.perform(invoices, AppNavItemInteraction.pin());
      UserInteraction.perform(orders, AppNavItemInteraction.unpin());

      assertEquals(List.of("Orders true 1", "Invoices true 2", "Orders false 1"), events);
      assertFalse(orders.isPinned());
      assertTrue(invoices.isPinned());
    }
  }

  @Test
  void shouldRejectPinningWithoutPinningOrNavigation() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AppNav nav = new AppNav();
      AppNavItem orders = new AppNavItem("Orders", "/orders");
      AppNavItem loose = new AppNavItem("Loose", "/loose");
      nav.addItem(orders);
      frame.add(nav, loose);

      Consumer<AppNavItem> pin = AppNavItemInteraction.pin();
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(orders, pin)).getMessage().contains("without pinning"));
      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.click(loose))
              .getMessage().contains("is not inside a navigation"));
    }
  }

  @Test
  void shouldRejectPinningTheGroupHeader() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AppNav nav = new AppNav();
      nav.getPinning().setEnabled(true);
      AppNavItem group = new AppNavItem("Reports", "/reports");
      group.addItem(new AppNavItem("Sales", "/reports/sales"));
      nav.addItem(group);
      List<String> events = new ArrayList<>();
      nav.onPin(event -> events.add(event.getKey()));
      frame.add(nav);
      Consumer<AppNavItem> pin = AppNavItemInteraction.pin();

      InteractionRejectedException rejection = assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(group, pin));

      assertTrue(rejection.getMessage()
          .startsWith("The component AppNavItem is a group header and a user cannot pin it."));
      assertFalse(group.isPinned());
      assertEquals(List.of(), events);
    }
  }

  @Test
  void shouldRejectPinningTheLeafWithoutPath() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      AppNav nav = new AppNav();
      nav.getPinning().setEnabled(true);
      AppNavItem help = new AppNavItem("Help");
      nav.addItem(help);
      List<String> events = new ArrayList<>();
      nav.onPin(event -> events.add(event.getKey()));
      frame.add(nav);
      Consumer<AppNavItem> pin = AppNavItemInteraction.pin();

      InteractionRejectedException rejection = assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(help, pin));

      assertTrue(rejection.getMessage()
          .startsWith("The component AppNavItem has no path and a user cannot pin it."));
      assertFalse(help.isPinned());
      assertEquals(List.of(), events);
    }
  }
}
