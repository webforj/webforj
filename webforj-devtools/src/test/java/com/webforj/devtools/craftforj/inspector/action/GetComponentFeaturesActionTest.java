package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class GetComponentFeaturesActionTest {

  @Test
  @DisplayName("Should return correct action name with inspector prefix")
  void shouldReturnCorrectActionName() {
    GetComponentFeaturesAction action = new GetComponentFeaturesAction();
    assertEquals("inspector.getComponentFeatures", action.getAction());
  }

  @Nested
  @DisplayName("Parameter validation")
  class ParameterValidation {

    @Test
    @DisplayName("Should throw exception when id is missing")
    void shouldThrowWhenIdMissing() {
      GetComponentFeaturesAction action = new GetComponentFeaturesAction();

      CraftforjActionException ex =
          assertThrows(CraftforjActionException.class, () -> action.handle(new JsonObject()));
      assertEquals("id is required", ex.getMessage());
    }

    @Test
    @DisplayName("Should throw exception when id is empty")
    void shouldThrowWhenIdEmpty() {
      GetComponentFeaturesAction action = new GetComponentFeaturesAction();
      JsonObject params = new JsonObject();
      params.addProperty("id", "");

      CraftforjActionException ex =
          assertThrows(CraftforjActionException.class, () -> action.handle(params));
      assertEquals("id is required", ex.getMessage());
    }
  }
}
