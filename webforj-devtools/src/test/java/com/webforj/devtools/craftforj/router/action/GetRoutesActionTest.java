package com.webforj.devtools.craftforj.router.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.router.RouteCollector;
import com.webforj.devtools.craftforj.router.model.RouteInfo;
import com.webforj.devtools.craftforj.router.model.RouteType;
import com.webforj.devtools.craftforj.router.model.SecurityAccess;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GetRoutesActionTest {

  @Test
  @DisplayName("Should return collected routes")
  void shouldReturnCollectedRoutes() {
    RouteInfo route = new RouteInfo();
    route.setId("test-id");
    route.setPath("/products");
    route.setComponentType("com.example.ProductView");
    route.setDisplayName("ProductView");
    route.setType(RouteType.VIEW);
    route.setOutletType("com.example.MainLayout");
    route.setPriority(10);
    route.setFrameTitle("Products");
    route.setParams(Collections.emptyList());
    route.setAliases(Collections.emptyList());
    route.setSecurity(SecurityAccess.NONE);
    route.setAllowedRoles(Collections.emptyList());
    route.setHasWillEnter(false);
    route.setHasDidEnter(true);
    route.setHasWillLeave(false);
    route.setHasDidLeave(false);
    route.setHasActivate(false);
    route.setSourceFile("/path/to/source.java");
    route.setActive(false);

    RouteCollector collector = mock(RouteCollector.class);
    when(collector.collectRoutes()).thenReturn(List.of(route));

    GetRoutesAction action = new GetRoutesAction(collector);
    GetRoutesAction.Response response = action.handle(new JsonObject());

    assertEquals(1, response.getRoutes().size());
    assertEquals("test-id", response.getRoutes().get(0).getId());
    assertEquals("/products", response.getRoutes().get(0).getPath());
  }

  @Test
  @DisplayName("Should return an empty response when no routes exist")
  void shouldReturnEmptyResponse() {
    RouteCollector collector = mock(RouteCollector.class);
    when(collector.collectRoutes()).thenReturn(Collections.emptyList());

    GetRoutesAction action = new GetRoutesAction(collector);

    assertTrue(action.handle(new JsonObject()).getRoutes().isEmpty());
  }

  @Test
  @DisplayName("Should propagate a collector failure to the dispatcher")
  void shouldPropagateCollectorFailure() {
    RouteCollector collector = mock(RouteCollector.class);
    when(collector.collectRoutes()).thenThrow(new IllegalStateException("registry gone"));

    GetRoutesAction action = new GetRoutesAction(collector);

    assertThrows(IllegalStateException.class, () -> action.handle(new JsonObject()));
  }
}
