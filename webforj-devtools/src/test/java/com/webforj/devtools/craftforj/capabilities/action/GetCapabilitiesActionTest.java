package com.webforj.devtools.craftforj.capabilities.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonObject;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GetCapabilitiesActionTest {

  @Test
  @DisplayName("Should return correct action name")
  void shouldReturnCorrectActionName() {
    GetCapabilitiesAction action = new GetCapabilitiesAction(null, true, List.of());
    assertEquals("capabilities.getCapabilities", action.getAction());
  }

  @Test
  @DisplayName("Should return version and capabilities")
  void shouldReturnVersionAndCapabilities() {
    List<String> capabilities = List.of("sourceCodeChanges");
    GetCapabilitiesAction action = new GetCapabilitiesAction("25.12-SNAPSHOT", true, capabilities);

    GetCapabilitiesAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertEquals("25.12-SNAPSHOT", response.getVersion());
    assertTrue(response.isLicensed());
    assertEquals(1, response.getCapabilities().size());
    assertTrue(response.getCapabilities().contains("sourceCodeChanges"));
  }

  @Test
  @DisplayName("Should return empty capabilities when none supported")
  void shouldReturnEmptyCapabilities() {
    GetCapabilitiesAction action = new GetCapabilitiesAction("25.10", true, List.of());

    GetCapabilitiesAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertEquals("25.10", response.getVersion());
    assertTrue(response.isLicensed());
    assertTrue(response.getCapabilities().isEmpty());
  }

  @Test
  @DisplayName("Should handle null version")
  void shouldHandleNullVersion() {
    GetCapabilitiesAction action = new GetCapabilitiesAction(null, true, List.of());

    GetCapabilitiesAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertEquals(null, response.getVersion());
    assertTrue(response.getCapabilities().isEmpty());
  }

  @Test
  @DisplayName("Should return the hotswap state of the run")
  void shouldReturnTheHotswapState() {
    GetCapabilitiesAction action =
        new GetCapabilitiesAction("26.02", true, List.of(), "full", "hotswapAgent", "limited");

    GetCapabilitiesAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertEquals("hotswapAgent", response.getHotswapTool());
    assertEquals("limited", response.getHotswapLevel());
  }

  @Test
  @DisplayName("Should answer without a hotswap state when no tool is attached")
  void shouldAnswerWithoutTheHotswapState() {
    GetCapabilitiesAction action = new GetCapabilitiesAction("26.02", true, List.of());

    GetCapabilitiesAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertNull(response.getHotswapTool());
    assertNull(response.getHotswapLevel());
  }

  @Test
  @DisplayName("Should handle null params")
  void shouldHandleNullParams() {
    GetCapabilitiesAction action =
        new GetCapabilitiesAction("25.12", true, List.of("sourceCodeChanges"));

    GetCapabilitiesAction.Response response = action.handle(null);

    assertNotNull(response);
    assertEquals("25.12", response.getVersion());
  }

  @Test
  @DisplayName("Should report unlicensed status")
  void shouldReportUnlicensedStatus() {
    GetCapabilitiesAction action = new GetCapabilitiesAction("25.12", false, List.of());

    GetCapabilitiesAction.Response response = action.handle(new JsonObject());

    assertNotNull(response);
    assertFalse(response.isLicensed());
    assertTrue(response.getCapabilities().isEmpty());
  }
}
