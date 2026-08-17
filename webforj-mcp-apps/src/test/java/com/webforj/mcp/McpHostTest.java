package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.component.Component;
import com.webforj.conceiver.Conceiver;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.router.RouteRegistry;
import com.webforj.router.RouteRelation;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.modelcontextprotocol.spec.McpSchema.ListResourcesResult;
import io.modelcontextprotocol.spec.McpSchema.TextContent;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

class McpHostTest {

  private final Page page = mock(Page.class);
  private final McpHost host = new McpHost(page);

  @Test
  @DisplayName("Should deliver the complete tool arguments to the view")
  void shouldDeliverToolInput() {
    AtomicReference<JsonNode> received = new AtomicReference<>();
    host.onToolInput(event -> received.set(event.getArguments()));

    host.dispatchHostMessage("""
        {
          "type": "tool-input",
          "payload": { "arguments": { "name": "Anna" } }
        }""");

    assertEquals("Anna", received.get().path("name").asString());
  }

  @Test
  @DisplayName("Should deliver the partial tool arguments to the view")
  void shouldDeliverToolInputPartial() {
    AtomicReference<JsonNode> received = new AtomicReference<>();
    host.onToolInputPartial(event -> received.set(event.getArguments()));

    host.dispatchHostMessage("""
        {
          "type": "tool-input-partial",
          "payload": { "arguments": { "na": "An" } }
        }""");

    assertEquals("An", received.get().path("na").asString());
  }

  @Test
  @DisplayName("Should deliver the tool result to the view")
  void shouldDeliverToolResult() {
    AtomicReference<CallToolResult> received = new AtomicReference<>();
    host.onToolResult(event -> received.set(event.getResult()));

    host.dispatchHostMessage("""
        {
          "type": "tool-result",
          "payload": { "structuredContent": { "_route": "/" } }
        }""");

    @SuppressWarnings("unchecked")
    Map<String, Object> structured = (Map<String, Object>) received.get().structuredContent();
    assertEquals("/", structured.get("_route"));
  }

  @Test
  @DisplayName("Should deliver the cancellation and its reason to the view")
  void shouldDeliverToolCancelled() {
    AtomicReference<String> received = new AtomicReference<>();
    host.onToolCancelled(event -> received.set(event.getReason()));

    host.dispatchHostMessage("""
        {
          "type": "tool-cancelled",
          "payload": { "reason": "user stopped" }
        }""");

    assertEquals("user stopped", received.get());
  }

  @Test
  @DisplayName("Should not navigate for the tool result opening the frame")
  void shouldNotNavigateForOpeningToolResult() {
    Router router = mock(Router.class);
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      host.dispatchHostMessage("""
          {
            "type": "tool-result",
            "payload": { "structuredContent": { "_route": "/orders" } }
          }""");

      verify(router, never()).navigate(any(Location.class));
    }
  }

  @Test
  @DisplayName("Should navigate for every tool result after the opening one")
  void shouldNavigateForFollowUpToolResults() {
    Router router = mock(Router.class);
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      host.dispatchHostMessage("""
          {
            "type": "tool-result",
            "payload": { "structuredContent": { "_route": "/orders" } }
          }""");
      host.dispatchHostMessage("""
          {
            "type": "tool-result",
            "payload": { "structuredContent": { "_route": "/orders" } }
          }""");
      host.dispatchHostMessage("""
          {
            "type": "tool-result",
            "payload": { "structuredContent": { "_route": "/inventory" } }
          }""");

      verify(router, times(2)).navigate(any(Location.class));
    }
  }

  @Test
  @DisplayName("Should keep the host context current with change notifications")
  void shouldTrackHostContext() {
    host.dispatchHostMessage("""
        {
          "type": "initialized",
          "payload": { "hostContext": { "theme": "light" } }
        }""");

    AtomicReference<JsonNode> received = new AtomicReference<>();
    host.onHostContextChanged(event -> received.set(event.getChanges()));
    host.dispatchHostMessage("""
        {
          "type": "host-context-changed",
          "payload": { "theme": "dark" }
        }""");

    assertEquals("dark", received.get().path("theme").asString());
    assertEquals("dark", host.getHostContext().orElseThrow().theme());
  }

  @Test
  @DisplayName("Should carry the handshake identity and capabilities")
  void shouldCarryHandshake() {
    host.dispatchHostMessage("""
        {
          "type": "initialized",
          "payload": {
            "hostInfo": { "name": "test-host", "version": "1" },
            "hostCapabilities": {
              "openLinks": {},
              "serverTools": { "listChanged": true },
              "message": { "text": {} },
              "sampling": { "tools": {} },
              "sandbox": { "csp": { "connectDomains": ["https://app.example.com"] } }
            }
          }
        }""");

    assertEquals("test-host", host.getHostInfo().orElseThrow().name());
    McpHostCapabilities capabilities = host.getHostCapabilities().orElseThrow();
    assertNotNull(capabilities.openLinks());
    assertTrue(capabilities.serverTools().listChanged());
    assertEquals(List.of("https://app.example.com"), capabilities.sandbox().csp().connectDomains());
    assertNotNull(capabilities.message().text());
    assertNotNull(capabilities.sampling().tools());
    assertNull(capabilities.logging());
    assertNull(capabilities.downloadFile());
  }

  @Test
  @DisplayName("Should type the context the host reports")
  void shouldTypeHostContext() {
    host.dispatchHostMessage("""
        {
          "type": "initialized",
          "payload": {
            "hostContext": {
              "theme": "dark",
              "displayMode": "fullscreen",
              "locale": "de-DE",
              "containerDimensions": { "maxHeight": 5000, "width": 760 },
              "deviceCapabilities": { "touch": false, "hover": true }
            }
          }
        }""");

    McpHostContext context = host.getHostContext().orElseThrow();
    assertEquals("dark", context.theme());
    assertEquals(McpAppDisplayMode.FULLSCREEN, context.displayMode());
    assertEquals("de-DE", context.locale());
    assertEquals(5000, context.containerDimensions().maxHeight().intValue());
    assertEquals(760, context.containerDimensions().width().intValue());
    assertTrue(context.deviceCapabilities().hover());
  }

  @Test
  @DisplayName("Should complete a call with the answer the host sent back")
  void shouldCompleteCallWithAnswer() {
    PendingResult<CallToolResult> pending = host.callTool("greet", Map.of("name", "Anna"));

    ArgumentCaptor<String> js = ArgumentCaptor.forClass(String.class);
    verify(page).executeJsVoidAsync(js.capture());
    assertTrue(js.getValue().contains("tools/call"));
    String callId = js.getValue().replaceAll(".*request\\(\"([^\"]+)\".*", "$1");

    AtomicReference<CallToolResult> answer = new AtomicReference<>();
    pending.thenAccept(answer::set);
    host.dispatchHostMessage("""
        {
          "type": "response",
          "callId": "%s",
          "result": { "content": [{ "type": "text", "text": "done" }] },
          "error": null
        }""".formatted(callId));

    assertNotNull(answer.get());
    assertEquals(1, answer.get().content().size());
  }

  @Test
  @DisplayName("Should fail a call with the error the host sent back")
  void shouldFailCallWithError() {
    PendingResult<Void> pending = host.sendMessage("hello");

    ArgumentCaptor<String> js = ArgumentCaptor.forClass(String.class);
    verify(page).executeJsVoidAsync(js.capture());
    assertTrue(js.getValue().contains("ui/message"));
    String callId = js.getValue().replaceAll(".*request\\(\"([^\"]+)\".*", "$1");

    AtomicReference<Throwable> failure = new AtomicReference<>();
    pending.exceptionally(thrown -> {
      failure.set(thrown);
      return null;
    });
    host.dispatchHostMessage("""
        {
          "type": "response",
          "callId": "%s",
          "result": null,
          "error": { "code": -32601, "message": "no" }
        }""".formatted(callId));

    assertNotNull(failure.get());
  }

  @Test
  @DisplayName("Should send conversation and context content as arrays of content blocks")
  void shouldSendContentAsBlocks() {
    host.sendMessage("hello");
    host.updateModelContext("state");

    ArgumentCaptor<String> js = ArgumentCaptor.forClass(String.class);
    verify(page, times(2)).executeJsVoidAsync(js.capture());
    assertTrue(js.getAllValues().get(0).contains("\"content\":[{"));
    assertTrue(js.getAllValues().get(1).contains("\"content\":[{"));
  }

  @Test
  @DisplayName("Should ask the host for a display mode by its wire value")
  void shouldRequestDisplayMode() {
    host.requestDisplayMode(McpAppDisplayMode.FULLSCREEN);

    verify(page).executeJsVoidAsync(contains("\"fullscreen\""));
  }

  @Test
  @DisplayName("Should answer a display mode request with the mode the host settled on")
  void shouldAnswerDisplayModeRequestWithSettledMode() {
    PendingResult<McpAppDisplayMode> pending = host.requestDisplayMode(McpAppDisplayMode.PIP);

    ArgumentCaptor<String> js = ArgumentCaptor.forClass(String.class);
    verify(page).executeJsVoidAsync(js.capture());
    String callId = js.getValue().replaceAll(".*request\\(\"([^\"]+)\".*", "$1");

    AtomicReference<McpAppDisplayMode> settled = new AtomicReference<>();
    pending.thenAccept(settled::set);
    host.dispatchHostMessage("""
        {
          "type": "response",
          "callId": "%s",
          "result": { "mode": "inline" },
          "error": null
        }""".formatted(callId));

    assertEquals(McpAppDisplayMode.INLINE, settled.get());
  }

  @Test
  @DisplayName("Should refuse a display mode request naming no mode")
  void shouldRefuseDisplayModeRequestWithoutMode() {
    IllegalArgumentException thrown =
        assertThrows(IllegalArgumentException.class, () -> host.requestDisplayMode(null));

    assertTrue(thrown.getMessage().contains("display mode"));
  }

  @Test
  @DisplayName("Should discard an unreadable host message")
  void shouldDiscardUnreadableMessage() {
    assertDoesNotThrow(() -> host.dispatchHostMessage("not json at all"));
    assertDoesNotThrow(() -> host.dispatchHostMessage("[1,2,3]"));
  }

  @Test
  @DisplayName("Should flush the channel when the application is ready")
  void shouldSignalReady() {
    host.signalReady();

    verify(page).executeJsVoidAsync(contains("__webforjMcpChannel.ready()"));
  }

  @Test
  @DisplayName("Should drop the listeners and fail the open calls on destroy")
  void shouldDropListenersAndFailOpenCallsOnDestroy() {
    AtomicReference<JsonNode> received = new AtomicReference<>();
    host.onToolInput(event -> received.set(event.getArguments()));
    PendingResult<ListResourcesResult> pending = host.listResources();
    AtomicReference<Throwable> failure = new AtomicReference<>();
    pending.exceptionally(thrown -> {
      failure.set(thrown);
      return null;
    });

    host.destroy();

    host.dispatchHostMessage("""
        {
          "type": "tool-input",
          "payload": { "arguments": { "name": "Anna" } }
        }""");
    assertNull(received.get());
    assertNotNull(failure.get());
  }

  @Test
  @DisplayName("Should answer a server call with the result of the rendered view")
  void shouldAnswerServerCall() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));

    Router router = routerShowing(McpTestViews.LiveView.class, new McpTestViews.LiveView());
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result =
          live.answerToolCall("live", JsonMapper.shared().createObjectNode().put("q", "alpha"));

      assertEquals("live alpha", ((TextContent) result.content().get(0)).text());
    }
  }

  @Test
  @DisplayName("Should refuse a server call of a tool no view answers")
  void shouldRefuseServerCallForUnknownTool() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));

    CallToolResult result = live.answerToolCall("missing", JsonMapper.shared().createObjectNode());

    assertTrue(result.isError());
    assertTrue(((TextContent) result.content().get(0)).text().contains("not on screen"));
  }

  @Test
  @DisplayName("Should refuse a server call while the view is not rendered")
  void shouldRefuseServerCallWhileViewNotRendered() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));

    Router router = routerShowing(McpTestViews.LiveView.class, null);
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result = live.answerToolCall("live", JsonMapper.shared().createObjectNode());

      assertTrue(result.isError());
      assertTrue(((TextContent) result.content().get(0)).text().contains("not on screen"));
    }
  }

  @Test
  @DisplayName("Should answer with an error when the view fails")
  void shouldAnswerObserverFailure() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));

    Router router =
        routerShowing(McpTestViews.FailingLiveView.class, new McpTestViews.FailingLiveView());
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result =
          live.answerToolCall("failing-live", JsonMapper.shared().createObjectNode());

      assertTrue(result.isError());
      assertTrue(((TextContent) result.content().get(0)).text().contains("the table is gone"));
    }
  }

  @Test
  @DisplayName("Should bind the instance token of the opening result for server calls")
  void shouldBindInstanceTokenForServerCalls() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));

    String token = McpAppInstances.deriveToken("session-bind", "live");
    live.dispatchHostMessage("""
        {
          "type": "tool-result",
          "payload": { "structuredContent": { "_route": "/live" },
            "_meta": { "webforj/instance": "%s" } }
        }""".formatted(token));

    Router router = routerShowing(McpTestViews.LiveView.class, new McpTestViews.LiveView());
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result = McpAppInstances.answerUpdateCall("session-bind", "live",
          JsonMapper.shared().createObjectNode().put("q", "beta"));

      assertEquals("live beta", ((TextContent) result.content().get(0)).text());
    }
  }

  @Test
  @DisplayName("Should refuse a server call of a session that opened no view")
  void shouldRefuseServerCallWithoutOpenView() {
    CallToolResult result = McpAppInstances.answerUpdateCall("session-unopened", "live",
        JsonMapper.shared().createObjectNode());

    assertTrue(result.isError());
    assertTrue(((TextContent) result.content().get(0)).text().contains("not open"));
  }

  @Test
  @DisplayName("Should unbind the instance on destroy")
  void shouldUnbindInstanceOnDestroy() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));

    String token = McpAppInstances.deriveToken("session-destroy", "live");
    live.dispatchHostMessage("""
        {
          "type": "tool-result",
          "payload": { "structuredContent": { "_route": "/live" },
            "_meta": { "webforj/instance": "%s" } }
        }""".formatted(token));

    live.destroy();
    CallToolResult result = McpAppInstances.answerUpdateCall("session-destroy", "live",
        JsonMapper.shared().createObjectNode());

    assertTrue(result.isError());
  }

  @Test
  @DisplayName("Should answer with a refusal when the session terminates mid call")
  void shouldRefuseWhenSessionTerminatesMidCall() {
    McpHost live = new McpHost(page, task -> {
      PendingResult<CallToolResult> pending = new PendingResult<>();
      pending.cancel();
      return pending;
    });

    Router router = routerShowing(McpTestViews.LiveView.class, new McpTestViews.LiveView());
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result = live.answerToolCall("live", JsonMapper.shared().createObjectNode());

      assertTrue(result.isError());
      assertTrue(((TextContent) result.content().get(0)).text().contains("terminated"));
    }
  }

  @Test
  @DisplayName("Should dispatch an action to the rendered view")
  void shouldDispatchActionToRenderedView() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));
    McpAppActionDescriptor action = actionNamed("actions_filter");
    Router router = routerShowing(McpTestViews.ActionsView.class, new McpTestViews.ActionsView());
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result = live.answerActionCall("actions", action,
          JsonMapper.shared().createObjectNode().put("query", "late").put("limit", 4));

      assertEquals("filtered late", ((TextContent) result.content().get(0)).text());
    }
  }

  @Test
  @DisplayName("Should confirm a void action")
  void shouldConfirmVoidAction() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));
    McpAppActionDescriptor action = actionNamed("actions_refresh");
    Router router = routerShowing(McpTestViews.ActionsView.class, new McpTestViews.ActionsView());
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result =
          live.answerActionCall("actions", action, JsonMapper.shared().createObjectNode());

      assertEquals("The 'actions_refresh' action completed.",
          ((TextContent) result.content().get(0)).text());
    }
  }

  @Test
  @DisplayName("Should return the structured value of an action")
  void shouldReturnActionStructuredValue() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));
    McpAppActionDescriptor action = actionNamed("actions_summarize");
    Router router = routerShowing(McpTestViews.ActionsView.class, new McpTestViews.ActionsView());
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result = live.answerActionCall("actions", action,
          JsonMapper.shared().createObjectNode().put("query", "late").put("limit", 4));

      assertEquals(new McpTestViews.ActionSummary("late", 4), result.structuredContent());
    }
  }

  @Test
  @DisplayName("Should resolve an external actions class through the conceiver")
  void shouldResolveExternalActionsClass() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));
    RouteRegistry registry = new RouteRegistry();
    registry.register("/external-actions", McpTestViews.ExternalActionsView.class);
    McpAppActionDescriptor action =
        McpAppRegistry.ofRegistry(registry).getDescriptors().get(0).getActionDescriptors().get(0);
    Router router = routerShowing(McpTestViews.ExternalActionsView.class,
        new McpTestViews.ExternalActionsView());
    Conceiver conceiver = mock(Conceiver.class);
    when(conceiver.get(McpTestViews.ExternalActions.class))
        .thenReturn(new McpTestViews.ExternalActions());
    try (MockedStatic<Router> routers = mockStatic(Router.class);
        MockedStatic<ConceiverProvider> conceivers = mockStatic(ConceiverProvider.class)) {
      routers.when(Router::getCurrent).thenReturn(router);
      conceivers.when(ConceiverProvider::getCurrent).thenReturn(conceiver);

      CallToolResult result = live.answerActionCall("external-actions", action,
          JsonMapper.shared().createObjectNode().put("query", "ignored").put("limit", 9));

      assertFalse(result.isError());
      assertEquals(new McpTestViews.ActionSummary("ExternalActionsView", 9),
          result.structuredContent());
    }
  }

  @Test
  @DisplayName("Should deliver opening arguments to an input method")
  void shouldDeliverOpeningArgumentsToInputMethod() {
    McpTestViews.InputMethodView view = new McpTestViews.InputMethodView();
    Router router = routerShowing(McpTestViews.InputMethodView.class, view);
    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      host.dispatchHostMessage("""
          {
            "type": "tool-input",
            "payload": { "arguments": { "query": "late", "limit": 4 } }
          }""");

      assertEquals(new McpTestViews.ActionInput("late", 4), view.getOpeningInput());
    }
  }

  @Test
  @DisplayName("Should deliver opening arguments to the active view instead of a cached view")
  void shouldDeliverOpeningArgumentsToActiveView() {
    McpTestViews.InputMethodView cached = new McpTestViews.InputMethodView();
    McpTestViews.SecondInputMethodView active = new McpTestViews.SecondInputMethodView();
    RouteRegistry registry = new RouteRegistry();
    registry.register(McpTestViews.InputMethodView.class);
    registry.register(McpTestViews.SecondInputMethodView.class);
    RouteRenderer renderer = mock(RouteRenderer.class);
    when(renderer.getRenderedComponent(McpTestViews.InputMethodView.class))
        .thenReturn(Optional.of(cached));
    when(renderer.getRenderedComponent(McpTestViews.SecondInputMethodView.class))
        .thenReturn(Optional.of(active));
    when(renderer.getActiveRoutePath())
        .thenReturn(Optional.of(new RouteRelation<>(McpTestViews.SecondInputMethodView.class)));
    Router router = mock(Router.class);
    when(router.getRegistry()).thenReturn(registry);
    when(router.getRenderer()).thenReturn(renderer);

    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      host.dispatchHostMessage("""
          {
            "type": "tool-input",
            "payload": { "arguments": { "query": "active", "limit": 6 } }
          }""");

      assertNull(cached.getOpeningInput());
      assertEquals(new McpTestViews.ActionInput("active", 6), active.getOpeningInput());
    }
  }

  @Test
  @DisplayName("Should refuse an action for a cached view that is no longer active")
  void shouldRefuseActionForCachedInactiveView() {
    McpHost live = new McpHost(page, task -> PendingResult.completedWith(task.get()));
    McpAppActionDescriptor action = actionNamed("actions_filter");
    RouteRegistry registry = new RouteRegistry();
    registry.register(McpTestViews.ActionsView.class);
    registry.register(McpTestViews.InputMethodView.class);
    RouteRenderer renderer = mock(RouteRenderer.class);
    when(renderer.getRenderedComponent(McpTestViews.ActionsView.class))
        .thenReturn(Optional.of(new McpTestViews.ActionsView()));
    when(renderer.getRenderedComponent(McpTestViews.InputMethodView.class))
        .thenReturn(Optional.of(new McpTestViews.InputMethodView()));
    when(renderer.getActiveRoutePath())
        .thenReturn(Optional.of(new RouteRelation<>(McpTestViews.InputMethodView.class)));
    Router router = mock(Router.class);
    when(router.getRegistry()).thenReturn(registry);
    when(router.getRenderer()).thenReturn(renderer);

    try (MockedStatic<Router> routers = mockStatic(Router.class)) {
      routers.when(Router::getCurrent).thenReturn(router);

      CallToolResult result = live.answerActionCall("actions", action,
          JsonMapper.shared().createObjectNode().put("query", "stale").put("limit", 1));

      assertTrue(result.isError());
      assertTrue(((TextContent) result.content().get(0)).text().contains("not on screen"));
    }
  }

  private static McpAppActionDescriptor actionNamed(String toolName) {
    RouteRegistry registry = new RouteRegistry();
    registry.register("/actions", McpTestViews.ActionsView.class);

    return McpAppRegistry.ofRegistry(registry).getDescriptors().get(0).getActionDescriptors()
        .stream().filter(action -> action.getToolName().equals(toolName)).findFirst().orElseThrow();
  }

  private static Router routerShowing(Class<? extends Component> viewClass, Component rendered) {
    RouteRegistry registry = new RouteRegistry();
    registry.register(viewClass);
    RouteRenderer renderer = mock(RouteRenderer.class);
    when(renderer.getRenderedComponent(viewClass)).thenReturn(Optional.ofNullable(rendered));
    when(renderer.getActiveRoutePath()).thenReturn(Optional.of(new RouteRelation<>(viewClass)));
    Router router = mock(Router.class);
    when(router.getRegistry()).thenReturn(registry);
    when(router.getRenderer()).thenReturn(renderer);

    return router;
  }
}
