package com.webforj.devtools.craftforj.router.action;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class NavigateToRouteActionTest {

  @Test
  @DisplayName("Should throw exception when no router available and no path/componentType")
  void shouldThrowWhenNoPathOrComponentType() {
    NavigateToRouteAction action = new NavigateToRouteAction();

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(new JsonObject()));
    assertTrue(ex.getMessage().contains("No router available"));
  }

  @Test
  @DisplayName("Should throw exception when componentType is empty and no path")
  void shouldThrowWhenComponentTypeEmpty() {
    NavigateToRouteAction action = new NavigateToRouteAction();
    JsonObject params = new JsonObject();
    params.addProperty("componentType", "");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertNotNull(ex.getMessage());
  }

  @Test
  @DisplayName("Should throw no router exception when path provided but no router")
  void shouldThrowNoRouterExceptionForPath() {
    NavigateToRouteAction action = new NavigateToRouteAction();
    JsonObject params = new JsonObject();
    params.addProperty("path", "/products/123");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("No router available"));
  }
}
