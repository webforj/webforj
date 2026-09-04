package com.webforj.devtools.craftforj.module.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import com.google.gson.JsonObject;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.module.ModuleStore;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class GetModuleActionTest {

  private static final String RESOURCE = "META-INF/resources/webforj/craftforj-ui.min.js";
  private static final ModuleStore STORE =
      new ModuleStore(Map.of("ui", RESOURCE, "missing", "META-INF/resources/webforj/missing.js"));

  private MockedStatic<Environment> environment;

  // Assets resolves the class loader through the running environment, which no unit test has.
  @BeforeEach
  void setUp() {
    environment = mockStatic(Environment.class);
    Environment current = mock(Environment.class);
    environment.when(Environment::getCurrent).thenReturn(current);
  }

  @AfterEach
  void tearDown() {
    environment.close();
  }

  @Test
  @DisplayName("Should expose the getModule action name")
  void shouldExposeActionName() {
    assertEquals("devtools.getModule", new GetModuleAction(STORE).getAction());
  }

  @Test
  @DisplayName("Should stream chunks that reassemble to the advertised total")
  void shouldReportTotalMatchingPayload() {
    GetModuleAction action = new GetModuleAction(STORE);

    StringBuilder assembled = new StringBuilder();
    int total;
    do {
      JsonObject params = request("ui");
      params.addProperty("offset", assembled.length());
      Map<String, Object> response = action.handle(params);
      total = (int) response.get("total");
      assertTrue(payload(response).length() > 0);
      assembled.append(payload(response));
    } while (assembled.length() < total);

    assertEquals(total, assembled.length());
  }

  @Test
  @DisplayName("Should answer every chunk with the digest of the whole module")
  void shouldAnswerWithTheDigest() {
    GetModuleAction action = new GetModuleAction(STORE);
    JsonObject params = request("ui");
    params.addProperty("length", 0);

    Map<String, Object> response = action.handle(params);

    assertEquals(STORE.read("ui").getSha256(), response.get("sha256"));
    assertEquals("", payload(response));
  }

  @Test
  @DisplayName("Should throw when the module resource is missing")
  void shouldThrowWhenResourceMissing() {
    GetModuleAction action = new GetModuleAction(STORE);

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(request("missing")));
    assertTrue(ex.getMessage().contains("not found"));
  }

  @Test
  @DisplayName("Should throw when the requested module is not in the catalog")
  void shouldThrowWhenModuleUnknown() {
    GetModuleAction action = new GetModuleAction(STORE);

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(request("nonsense")));
    assertTrue(ex.getMessage().contains("Unknown craftforJ module"));
  }

  @Test
  @DisplayName("Should throw when the request carries no module name")
  void shouldThrowWhenNameMissing() {
    GetModuleAction action = new GetModuleAction(STORE);

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(new JsonObject()));
    assertTrue(ex.getMessage().contains("no name"));
  }

  private static JsonObject request(String name) {
    JsonObject params = new JsonObject();
    params.addProperty("name", name);

    return params;
  }

  private static String payload(Map<String, Object> response) {
    return (String) response.get("chunk");
  }
}
