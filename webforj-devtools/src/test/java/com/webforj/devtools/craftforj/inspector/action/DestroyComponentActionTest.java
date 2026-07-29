package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.google.gson.JsonObject;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import java.util.Optional;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DestroyComponentActionTest {

  @Test
  @DisplayName("Should destroy component successfully")
  void shouldDestroyComponentSuccessfully() {
    Component mockComponent = mock(Component.class);
    DestroyComponentAction action = new DestroyComponentAction(id -> Optional.of(mockComponent));

    JsonObject params = new JsonObject();
    params.addProperty("componentId", "test-id");

    Void result = action.handle(params);

    assertNull(result);
    verify(mockComponent).destroy();
  }

  @Test
  @DisplayName("Should throw exception when componentId is missing")
  void shouldThrowWhenComponentIdMissing() {
    DestroyComponentAction action = new DestroyComponentAction(id -> Optional.empty());

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(new JsonObject()));
    assertTrue(ex.getMessage().contains("Missing componentId"));
  }

  @Test
  @DisplayName("Should throw exception when component not found")
  void shouldThrowWhenComponentNotFound() {
    DestroyComponentAction action = new DestroyComponentAction(id -> Optional.empty());

    JsonObject params = new JsonObject();
    params.addProperty("componentId", "non-existent");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("Component not found"));
  }
}
