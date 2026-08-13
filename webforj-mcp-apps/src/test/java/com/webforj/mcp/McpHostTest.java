package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.webforj.Page;
import com.webforj.PendingResult;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.modelcontextprotocol.spec.McpSchema.ListResourcesResult;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import tools.jackson.databind.JsonNode;

class McpHostTest {

  private final Page page = mock(Page.class);
  private final McpHost host = new McpHost(page);

  @Test
  @DisplayName("Should deliver the complete tool arguments to the view")
  void shouldDeliverToolInput() {
    AtomicReference<JsonNode> received = new AtomicReference<>();
    host.onToolInput(event -> received.set(event.getArguments()));

    host.dispatch("""
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

    host.dispatch("""
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

    host.dispatch("""
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

    host.dispatch("""
        {
          "type": "tool-cancelled",
          "payload": { "reason": "user stopped" }
        }""");

    assertEquals("user stopped", received.get());
  }

  @Test
  @DisplayName("Should keep the host context current with change notifications")
  void shouldTrackHostContext() {
    host.dispatch("""
        {
          "type": "initialized",
          "payload": { "hostContext": { "theme": "light" } }
        }""");

    AtomicReference<JsonNode> received = new AtomicReference<>();
    host.onHostContextChanged(event -> received.set(event.getChanges()));
    host.dispatch("""
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
    host.dispatch("""
        {
          "type": "initialized",
          "payload": {
            "hostInfo": { "name": "claude", "version": "1" },
            "hostCapabilities": {
              "openLinks": {},
              "serverTools": { "listChanged": true },
              "message": { "text": {} },
              "sampling": { "tools": {} },
              "sandbox": { "csp": { "connectDomains": ["https://app.example.com"] } }
            }
          }
        }""");

    assertEquals("claude", host.getHostInfo().orElseThrow().name());
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
    host.dispatch("""
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
    host.dispatch("""
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
    host.dispatch("""
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
    host.dispatch("""
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
    assertDoesNotThrow(() -> host.dispatch("not json at all"));
    assertDoesNotThrow(() -> host.dispatch("[1,2,3]"));
  }

  @Test
  @DisplayName("Should flush the channel when the application is ready")
  void shouldSignalReady() {
    host.ready();

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

    host.dispatch("""
        {
          "type": "tool-input",
          "payload": { "arguments": { "name": "Anna" } }
        }""");
    assertNull(received.get());
    assertNotNull(failure.get());
  }
}
