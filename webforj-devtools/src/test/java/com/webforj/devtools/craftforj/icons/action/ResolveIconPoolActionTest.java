package com.webforj.devtools.craftforj.icons.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.icons.action.ResolveIconPoolAction.Response;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

@DisplayName("ResolveIconPoolAction")
class ResolveIconPoolActionTest {

  private Page page;
  private ResolveIconPoolAction action;

  @BeforeEach
  void setUp() {
    page = mock(Page.class);
    action = new ResolveIconPoolAction() {
      @Override
      protected Page getPage() {
        return page;
      }
    };
  }

  private JsonObject params(String json) {
    return JsonParser.parseString(json).getAsJsonObject();
  }

  private Response handle(String paramsJson, Object jsResult) {
    when(page.executeJsAsync(anyString())).thenReturn(PendingResult.completedWith(jsResult));

    Object[] holder = new Object[1];
    action.handle(params(paramsJson)).thenAccept(value -> holder[0] = value);

    return (Response) holder[0];
  }

  @Test
  @DisplayName("uses the icons.resolve action name")
  void shouldUseActionName() {
    assertEquals("icons.resolve", action.getAction());
  }

  @Test
  @DisplayName("rejects requests without pool or names")
  void shouldRejectMissingParams() {
    assertThrows(CraftforjActionException.class, () -> action.handle(null));
    assertThrows(CraftforjActionException.class,
        () -> action.handle(params("{\"pool\":\"tabler\"}")));
    assertThrows(CraftforjActionException.class,
        () -> action.handle(params("{\"names\":[\"bell\"]}")));
  }

  @Test
  @DisplayName("resolves names through the page pool resolver")
  void shouldResolveThroughPagePool() {
    Response response = handle("{\"pool\":\"tabler\",\"names\":[\"bell\",\"home\"]}",
        "{\"bell\":\"data:image/svg+xml,b\",\"home\":\"https://cdn/home.svg\"}");

    assertEquals(Map.of("bell", "data:image/svg+xml,b", "home", "https://cdn/home.svg"),
        response.getIcons());

    ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
    verify(page).executeJsAsync(captor.capture());
    String script = captor.getValue();
    assertTrue(script.contains("window.Dwc"));
    assertTrue(script.contains("\"tabler\""));
    assertTrue(script.contains("\"bell\""));
    assertTrue(script.contains("\"home\""));
  }

  @Test
  @DisplayName("returns an empty map when the pool is not registered")
  void shouldReturnEmptyMapForUnknownPool() {
    Response response = handle("{\"pool\":\"nope\",\"names\":[\"bell\"]}", null);

    assertEquals(Map.of(), response.getIcons());
  }
}
