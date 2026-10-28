package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.automation.find.ComponentLocator;
import com.webforj.component.Component;
import com.webforj.component.button.Button;
import com.webforj.router.history.ParametersBag;
import com.webforj.testbed.fixture.AboutView;
import com.webforj.testbed.fixture.MainView;
import com.webforj.testbed.fixture.ProductView;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class NavigationTest {

  @AfterEach
  void tearDown() {
    WebforjTestbed.tearDown();
  }

  @Test
  void shouldNavigateToRegisteredRouteClasses() {
    WebforjTestbed context = WebforjTestbed
        .setup(new WebforjTestbedOptions().setRouteClasses(MainView.class, AboutView.class));

    MainView view = context.navigate(MainView.class);

    assertNotNull(view);
    assertTrue(view.isAttached());
    assertEquals("Hello", view.getButton().getText());
    assertTrue(ComponentLocator.of(Button.class).byText("Hello").isPresent());
  }

  @Test
  void shouldNavigateToTheRouteOfTheViewWithTheGivenParameters() {
    WebforjTestbed context =
        WebforjTestbed.setup(new WebforjTestbedOptions().setRouteClasses(ProductView.class));

    ProductView view = context.navigate(ProductView.class, ParametersBag.of("id=42"));

    assertEquals("42", view.getProductId());
    assertEquals("/product/42",
        context.getRouter().orElseThrow().getHistory().getLocation().orElseThrow().getFullURI());
  }

  @Test
  void shouldNavigateByLocation() {
    WebforjTestbed context = WebforjTestbed
        .setup(new WebforjTestbedOptions().setRouteClasses(MainView.class, AboutView.class));

    Component view = context.navigate("about");

    assertInstanceOf(AboutView.class, view);
  }

  @Test
  void shouldRunRouteLifecycleObservers() {
    WebforjTestbed context = WebforjTestbed
        .setup(new WebforjTestbedOptions().setRouteClasses(MainView.class, AboutView.class));

    AboutView about = context.navigate(AboutView.class);

    assertEquals(1, about.getEnterCount());
    assertEquals(0, about.getLeaveCount());

    context.navigate(MainView.class);

    assertEquals(1, about.getLeaveCount());
  }

  @Test
  void shouldTrackNavigationInHistory() {
    WebforjTestbed context = WebforjTestbed
        .setup(new WebforjTestbedOptions().setRouteClasses(MainView.class, AboutView.class));

    context.navigate(MainView.class);
    context.navigate(AboutView.class);

    assertEquals("/about",
        context.getRouter().orElseThrow().getHistory().getLocation().orElseThrow().getFullURI());
  }

  @Test
  void shouldDiscoverRoutesFromPackages() {
    WebforjTestbed context = WebforjTestbed
        .setup(new WebforjTestbedOptions().setRoutePackages("com.webforj.testbed.fixture"));

    assertNotNull(context.navigate(MainView.class));
  }

  @Test
  void shouldRegisterRoutesThroughTheConfigurer() {
    WebforjTestbed context = WebforjTestbed.setup(new WebforjTestbedOptions()
        .setRegistryConfigurer(registry -> registry.register(MainView.class)));

    assertNotNull(context.navigate(MainView.class));
  }

  @Test
  void shouldFailWithoutRouter() {
    WebforjTestbed context = WebforjTestbed.setup();

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> context.navigate(MainView.class));

    assertTrue(failure.getMessage().contains("No router"));
  }

  @Test
  void shouldFailWhenNavigationRendersAnotherView() {
    WebforjTestbed context = WebforjTestbed
        .setup(new WebforjTestbedOptions().setRouteClasses(MainView.class, AboutView.class));

    assertThrows(Exception.class, () -> context.navigate("missing"));
  }
}
