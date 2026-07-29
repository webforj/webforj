package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class SetFeaturePropertyActionTest {

  @Test
  @DisplayName("Should return correct action name with inspector prefix")
  void shouldReturnCorrectActionName() {
    SetFeaturePropertyAction action = new SetFeaturePropertyAction();
    assertEquals("inspector.setFeatureProperty", action.getAction());
  }

  @Nested
  @DisplayName("Parameter validation")
  class ParameterValidation {

    @Test
    @DisplayName("Should throw exception when id is missing")
    void shouldThrowWhenIdMissing() {
      SetFeaturePropertyAction action = new SetFeaturePropertyAction();
      JsonObject params = new JsonObject();
      params.addProperty("featureType", "HasText");
      params.addProperty("value", "test");

      CraftforjActionException ex =
          assertThrows(CraftforjActionException.class, () -> action.handle(params));
      assertEquals("id is required", ex.getMessage());
    }

    @Test
    @DisplayName("Should throw exception when id is empty")
    void shouldThrowWhenIdEmpty() {
      SetFeaturePropertyAction action = new SetFeaturePropertyAction();
      JsonObject params = new JsonObject();
      params.addProperty("id", "");
      params.addProperty("featureType", "HasText");

      CraftforjActionException ex =
          assertThrows(CraftforjActionException.class, () -> action.handle(params));
      assertEquals("id is required", ex.getMessage());
    }

    @Test
    @DisplayName("Should throw exception when featureType is missing")
    void shouldThrowWhenFeatureTypeMissing() {
      SetFeaturePropertyAction action = new SetFeaturePropertyAction();
      JsonObject params = new JsonObject();
      params.addProperty("id", "test-id");
      params.addProperty("value", "test");

      CraftforjActionException ex =
          assertThrows(CraftforjActionException.class, () -> action.handle(params));
      assertEquals("featureType is required", ex.getMessage());
    }

    @Test
    @DisplayName("Should throw exception when featureType is empty")
    void shouldThrowWhenFeatureTypeEmpty() {
      SetFeaturePropertyAction action = new SetFeaturePropertyAction();
      JsonObject params = new JsonObject();
      params.addProperty("id", "test-id");
      params.addProperty("featureType", "");

      CraftforjActionException ex =
          assertThrows(CraftforjActionException.class, () -> action.handle(params));
      assertEquals("featureType is required", ex.getMessage());
    }
  }

}
