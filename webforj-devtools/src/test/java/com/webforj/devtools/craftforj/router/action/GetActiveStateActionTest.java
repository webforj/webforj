package com.webforj.devtools.craftforj.router.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.router.ActiveRouteTracker;
import com.webforj.devtools.craftforj.router.model.ActiveRouteState;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GetActiveStateActionTest {

  @Test
  @DisplayName("Should return null state when tracker has no state")
  void shouldReturnNullStateWhenNoState() {
    ActiveRouteTracker tracker = mock(ActiveRouteTracker.class);
    when(tracker.getCurrentState()).thenReturn(null);

    GetActiveStateAction action = new GetActiveStateAction(tracker);
    GetActiveStateAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertNull(response.getState());
  }

  @Test
  @DisplayName("Should return current state from tracker")
  void shouldReturnCurrentState() {
    ActiveRouteState state = new ActiveRouteState();
    state.setCurrentPath("/products/123");
    state.setActiveRouteIds(List.of("com.example.ProductView:/products/:id"));
    state.setParams(Map.of("id", "123"));

    ActiveRouteTracker tracker = mock(ActiveRouteTracker.class);
    when(tracker.getCurrentState()).thenReturn(state);

    GetActiveStateAction action = new GetActiveStateAction(tracker);
    GetActiveStateAction.Response response = action.handle(new JsonObject());

    assertNotNull(response.getState());
    assertEquals("/products/123", response.getState().getCurrentPath());
    assertEquals(1, response.getState().getActiveRouteIds().size());
    assertEquals("123", response.getState().getParams().get("id"));
  }
}
