package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.router.RouteRegistry;
import com.webforj.router.RouteRelation;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.modelcontextprotocol.spec.McpSchema.TextContent;
import java.util.Optional;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import tools.jackson.databind.json.JsonMapper;

class McpAppInstancesTest {

  @Test
  @DisplayName("Should derive the same token for the same session and distinct tokens otherwise")
  void shouldDeriveDeterministicTokens() {
    assertEquals(McpAppInstances.deriveToken("session-a", "live"),
        McpAppInstances.deriveToken("session-a", "live"));
    assertNotEquals(McpAppInstances.deriveToken("session-a", "live"),
        McpAppInstances.deriveToken("session-b", "live"));
    assertNotEquals(McpAppInstances.deriveToken("session-a", "live"),
        McpAppInstances.deriveToken("session-a", "orders"));
  }

  @Test
  @DisplayName("Should never carry the session id inside the token")
  void shouldNotDiscloseSessionIdInToken() {
    assertFalse(McpAppInstances.deriveToken("session-secret", "live").contains("session-secret"));
  }

  @Test
  @DisplayName("Should hold no state for a session whose render never bound")
  void shouldHoldNoStateForUnboundRender() {
    McpAppInstances.deriveToken("session-abandoned", "live");

    CallToolResult result = McpAppInstances.answerUpdateCall("session-abandoned", "live",
        JsonMapper.shared().createObjectNode());

    assertTrue(result.isError());
    assertTrue(((TextContent) result.content().get(0)).text().contains("not open"));
  }

  @Test
  @DisplayName("Should keep the newer application of a session when the older frame dies late")
  void shouldKeepNewerBindingWhenOlderFrameDiesLate() {
    String token = McpAppInstances.deriveToken("session-replaced", "live");
    McpHost older = liveHost();
    McpHost newer = liveHost();
    McpAppInstances.bindInstance(token, older);
    McpAppInstances.bindInstance(token, newer);

    McpAppInstances.unbindInstance(token, older);

    Router router = liveRouter();
    try (MockedStatic<Router> routers = Mockito.mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result = McpAppInstances.answerUpdateCall("session-replaced", "live",
          JsonMapper.shared().createObjectNode().put("q", "kept"));

      assertEquals("live kept", ((TextContent) result.content().get(0)).text());
    }

    McpAppInstances.unbindInstance(token, newer);
    assertTrue(McpAppInstances
        .answerUpdateCall("session-replaced", "live", JsonMapper.shared().createObjectNode())
        .isError());
  }

  @Test
  @DisplayName("Should keep the bindings of two views in one session apart")
  void shouldKeepViewBindingsApart() {
    McpAppInstances.bindInstance(McpAppInstances.deriveToken("session-two-views", "live"),
        liveHost());

    CallToolResult result = McpAppInstances.answerUpdateCall("session-two-views", "orders",
        JsonMapper.shared().createObjectNode());

    assertTrue(result.isError());
    assertTrue(((TextContent) result.content().get(0)).text().contains("not open"));
  }

  private static McpHost liveHost() {
    return new McpHost(mock(Page.class), task -> PendingResult.completedWith(task.get()));
  }

  private static Router liveRouter() {
    RouteRegistry registry = new RouteRegistry();
    registry.register(McpTestViews.LiveView.class);
    RouteRenderer renderer = mock(RouteRenderer.class);
    when(renderer.getRenderedComponent(McpTestViews.LiveView.class))
        .thenReturn(Optional.of(new McpTestViews.LiveView()));
    when(renderer.getActiveRoutePath())
        .thenReturn(Optional.of(new RouteRelation<>(McpTestViews.LiveView.class)));
    Router router = mock(Router.class);
    when(router.getRegistry()).thenReturn(registry);
    when(router.getRenderer()).thenReturn(renderer);

    return router;
  }
}
