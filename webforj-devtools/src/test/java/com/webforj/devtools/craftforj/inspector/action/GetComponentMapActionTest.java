package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.model.ComponentMeta;
import com.webforj.devtools.craftforj.utilities.ComponentMapBuilder;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GetComponentMapActionTest {

  @Test
  @DisplayName("Should return correct action name with inspector prefix")
  void shouldReturnCorrectActionName() {
    GetComponentMapAction action = new GetComponentMapAction();
    assertEquals("inspector.getComponentMap", action.getAction());
  }

  @Test
  @DisplayName("Should return component map from builder")
  void shouldReturnComponentMapFromBuilder() {
    ComponentMapBuilder mockBuilder = mock(ComponentMapBuilder.class);
    ComponentMeta meta = new ComponentMeta("id-1", "com.example.Button", "com.example.Button",
        "Button", false, null, null);
    Map<String, List<ComponentMeta>> expectedMap = Map.of("dwc-1", List.of(meta));
    when(mockBuilder.buildComponentMap()).thenReturn(expectedMap);

    GetComponentMapAction action = new GetComponentMapAction(mockBuilder);
    GetComponentMapAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertNotNull(response.getComponents());
    assertEquals(1, response.getComponents().size());
    assertTrue(response.getComponents().containsKey("dwc-1"));
    assertEquals("Button", response.getComponents().get("dwc-1").get(0).getDisplayName());
  }

  @Test
  @DisplayName("Should return empty map when no components")
  void shouldReturnEmptyMapWhenNoComponents() {
    ComponentMapBuilder mockBuilder = mock(ComponentMapBuilder.class);
    when(mockBuilder.buildComponentMap()).thenReturn(Map.of());

    GetComponentMapAction action = new GetComponentMapAction(mockBuilder);
    GetComponentMapAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertNotNull(response.getComponents());
    assertTrue(response.getComponents().isEmpty());
  }

  @Test
  @DisplayName("Should handle null params")
  void shouldHandleNullParams() {
    ComponentMapBuilder mockBuilder = mock(ComponentMapBuilder.class);
    when(mockBuilder.buildComponentMap()).thenReturn(Map.of());

    GetComponentMapAction action = new GetComponentMapAction(mockBuilder);
    GetComponentMapAction.Response response = action.handle(null);

    assertNotNull(response);
  }
}
