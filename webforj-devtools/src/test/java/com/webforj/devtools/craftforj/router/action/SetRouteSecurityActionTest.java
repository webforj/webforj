package com.webforj.devtools.craftforj.router.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.router.RouteSecurityModifier;
import com.webforj.devtools.craftforj.router.model.SecurityAccess;
import com.webforj.router.RouteEntry;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

@DisplayName("SetRouteSecurityAction")
class SetRouteSecurityActionTest {

  private RouteSecurityModifier modifier;
  private SetRouteSecurityAction action;
  private Router mockRouter;
  private MockedStatic<Router> routerMock;
  private MockedStatic<SourceFileResolver> resolverMock;

  @BeforeEach
  void setUp() {
    modifier = mock(RouteSecurityModifier.class);
    action = new SetRouteSecurityAction(modifier);
    mockRouter = mock(Router.class);
    RouteRegistry registry = mock(RouteRegistry.class);
    when(mockRouter.getRegistry()).thenReturn(registry);
    when(registry.getAvailableRouteEntires())
        .thenReturn(List.of(new RouteEntry("/dashboard", DashboardView.class)));

    routerMock = mockStatic(Router.class);
    routerMock.when(Router::getCurrent).thenReturn(mockRouter);
    resolverMock = mockStatic(SourceFileResolver.class);
    resolverMock.when(() -> SourceFileResolver.resolve(anyString(), any()))
        .thenReturn("/project/src/main/java/DashboardView.java");
  }

  @AfterEach
  void tearDown() {
    routerMock.close();
    resolverMock.close();
  }

  private JsonObject createParams(String access) {
    JsonObject params = new JsonObject();
    params.addProperty("componentType", DashboardView.class.getName());
    params.addProperty("access", access);

    return params;
  }

  @Test
  @DisplayName("Should expose the action name")
  void shouldExposeActionName() {
    assertEquals("router.setSecurity", action.getAction());
  }

  @Test
  @DisplayName("Should apply security to the resolved source file")
  void shouldApplySecurity() {
    JsonObject params = createParams("PERMIT_ALL");

    action.handle(params);

    verify(modifier).apply(Path.of("/project/src/main/java/DashboardView.java"), "DashboardView",
        SecurityAccess.PERMIT_ALL, List.of());
  }

  @Test
  @DisplayName("Should pass roles for ROLES_ALLOWED")
  void shouldPassRoles() {
    JsonObject params = createParams("ROLES_ALLOWED");
    JsonArray roles = new JsonArray();
    roles.add("ADMIN");
    roles.add("MANAGER");
    params.add("roles", roles);

    action.handle(params);

    verify(modifier).apply(Path.of("/project/src/main/java/DashboardView.java"), "DashboardView",
        SecurityAccess.ROLES_ALLOWED, List.of("ADMIN", "MANAGER"));
  }

  @Test
  @DisplayName("Should throw when no router available")
  void shouldThrowWhenNoRouter() {
    routerMock.when(Router::getCurrent).thenReturn(null);

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(createParams("NONE")));
    assertTrue(ex.getMessage().contains("No router available"));
  }

  @Test
  @DisplayName("Should throw when componentType is missing")
  void shouldThrowWhenComponentTypeMissing() {
    JsonObject params = new JsonObject();
    params.addProperty("access", "NONE");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("componentType is required"));
  }

  @Test
  @DisplayName("Should throw when access is missing")
  void shouldThrowWhenAccessMissing() {
    JsonObject params = new JsonObject();
    params.addProperty("componentType", DashboardView.class.getName());

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("access is required"));
  }

  @Test
  @DisplayName("Should reject role names with quotes or backslashes")
  void shouldRejectInvalidRoleNames() {
    JsonObject params = createParams("ROLES_ALLOWED");
    JsonArray roles = new JsonArray();
    roles.add("AD\"MIN");
    params.add("roles", roles);

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("Invalid role name"));
  }

  @Test
  @DisplayName("Should throw when access is unknown")
  void shouldThrowWhenAccessUnknown() {
    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(createParams("INVALID")));
    assertTrue(ex.getMessage().contains("Unknown access"));
  }

  @Test
  @DisplayName("Should throw when componentType is not a registered route")
  void shouldThrowWhenRouteNotRegistered() {
    JsonObject params = createParams("PERMIT_ALL");
    params.addProperty("componentType", "com.example.Unknown");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("not a registered route"));
  }

  @Test
  @DisplayName("Should throw when no Java source file is found")
  void shouldThrowWhenSourceMissing() {
    resolverMock.when(() -> SourceFileResolver.resolve(anyString(), any())).thenReturn(null);

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(createParams("NONE")));
    assertTrue(ex.getMessage().contains("source file not found"));
  }

  @NodeName("dashboard-view")
  static class DashboardView extends ElementCompositeContainer {
  }
}
