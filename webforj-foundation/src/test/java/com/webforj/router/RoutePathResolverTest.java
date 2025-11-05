package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.component.Composite;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.router.annotation.Route;
import org.junit.jupiter.api.Test;

class RoutePathResolverTest {

  @Test
  void shouldReturnNullForNullInput() {
    assertNull(RoutePathResolver.resolvePath(null));
  }

  @Test
  void shouldReturnNullForNonRouteComponent() {
    assertNull(RoutePathResolver.resolvePath(TestComponent.class));
  }

  @Test
  void shouldResolveSimpleViewPath() {
    assertEquals("/login", RoutePathResolver.resolvePath(LoginView.class));
  }

  @Test
  void shouldResolveSimpleLayoutPath() {
    assertEquals("@main", RoutePathResolver.resolvePath(MainLayout.class));
  }

  @Test
  void shouldAutoGenerateViewPath() {
    assertEquals("dashboard", RoutePathResolver.resolvePath(DashboardView.class));
  }

  @Test
  void shouldAutoGenerateLayoutPath() {
    assertEquals("@admin", RoutePathResolver.resolvePath(AdminLayout.class));
  }

  @Test
  void shouldResolveNestedRoute() {
    assertEquals("@main/settings", RoutePathResolver.resolvePath(SettingsView.class));
  }

  @Test
  void shouldThrowForInvalidAutoDetect() {
    assertThrows(IllegalArgumentException.class,
        () -> RoutePathResolver.resolvePath(InvalidComponent.class));
  }

  @Test
  void shouldThrowForEmptyLayoutName() {
    assertThrows(IllegalArgumentException.class, () -> RoutePathResolver.resolvePath(Layout.class));
  }

  static class TestComponent extends Composite<ElementCompositeContainer> {
  }

  @Route("/login")
  static class LoginView extends Composite<ElementCompositeContainer> {
  }

  @Route("@main")
  static class MainLayout extends Composite<ElementCompositeContainer> {
  }

  @Route
  static class DashboardView extends Composite<ElementCompositeContainer> {
  }

  @Route
  static class AdminLayout extends Composite<ElementCompositeContainer> {
  }

  @Route(value = "settings", outlet = MainLayout.class)
  static class SettingsView extends Composite<ElementCompositeContainer> {
  }

  @Route
  static class InvalidComponent extends Composite<ElementCompositeContainer> {
  }

  @Route
  static class Layout extends Composite<ElementCompositeContainer> {
  }
}
