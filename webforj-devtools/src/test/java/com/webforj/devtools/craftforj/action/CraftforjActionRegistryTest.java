package com.webforj.devtools.craftforj.action;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonObject;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.devtools.craftforj.security.ChannelCredentials;
import com.webforj.event.page.PageEvent;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

class CraftforjActionRegistryTest {

  private static final ChannelCredentials CREDENTIALS =
      ChannelCredentials.of("test-nonce", "sink1");

  private CraftforjActionRegistry registry;

  @BeforeEach
  void setUp() {
    registry = new CraftforjActionRegistry(CREDENTIALS);
  }

  @Test
  void shouldRegisterHandler() {
    TestActionHandler handler = new TestActionHandler("testAction");
    registry.register(handler);
    assertTrue(registry.hasHandler("testAction"));
  }

  @Test
  void shouldThrowOnDuplicateRegistration() {
    TestActionHandler handler1 = new TestActionHandler("testAction");
    TestActionHandler handler2 = new TestActionHandler("testAction");
    registry.register(handler1);

    assertThrows(IllegalArgumentException.class, () -> registry.register(handler2));
  }

  @Test
  void shouldUnregisterHandler() {
    TestActionHandler handler = new TestActionHandler("testAction");
    registry.register(handler);
    assertTrue(registry.hasHandler("testAction"));

    boolean removed = registry.unregister("testAction");
    assertTrue(removed);
    assertFalse(registry.hasHandler("testAction"));
  }

  @Test
  void shouldReturnFalseWhenUnregisteringNonExistent() {
    boolean removed = registry.unregister("nonExistent");
    assertFalse(removed);
  }

  @Nested
  @DisplayName("dispatch")
  class Dispatch {

    private String dispatchAndCapture(String json) {
      Page page = mock(Page.class);
      PageEvent event = mock(PageEvent.class);
      when(event.getData()).thenReturn(Map.of("request", json));

      registry.dispatch(page, event);

      ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
      verify(page).executeJsVoidAsync(captor.capture());

      return captor.getValue();
    }

    private String request(String requestId, String action) {
      return request(requestId, action, CREDENTIALS.getNonce());
    }

    private String request(String requestId, String action, String nonce) {
      return "{\"requestId\":\"" + requestId + "\",\"action\":\"" + action + "\",\"nonce\":\""
          + nonce + "\"}";
    }

    @Test
    @DisplayName("Should ignore a request that carries no nonce")
    void shouldIgnoreRequestWithoutNonce() {
      registry.register(new TestActionHandler("testAction"));

      Page page = mock(Page.class);
      PageEvent event = mock(PageEvent.class);
      when(event.getData())
          .thenReturn(Map.of("request", "{\"requestId\":\"r1\",\"action\":\"testAction\"}"));

      registry.dispatch(page, event);

      verify(page, never()).executeJsVoidAsync(anyString());
    }

    @Test
    @DisplayName("Should ignore a request that carries the wrong nonce")
    void shouldIgnoreRequestWithWrongNonce() {
      registry.register(new TestActionHandler("testAction"));

      Page page = mock(Page.class);
      PageEvent event = mock(PageEvent.class);
      when(event.getData()).thenReturn(Map.of("request", request("r1", "testAction", "guessed")));

      registry.dispatch(page, event);

      verify(page, never()).executeJsVoidAsync(anyString());
    }

    @Test
    @DisplayName("Should call the response sink named by the credentials")
    void shouldCallTheNamedResponseSink() {
      registry.register(new TestActionHandler("testAction"));

      String response = dispatchAndCapture(request("r1", "testAction"));

      assertTrue(response.startsWith("window.__webforjDevToolsResponse_sink1 &&"));
    }

    @Test
    @DisplayName("Should answer malformed JSON with an error response")
    void shouldAnswerMalformedJson() {
      assertTrue(dispatchAndCapture("{").contains("malformed"));
      assertTrue(dispatchAndCapture("[1]").contains("malformed"));
    }

    @Test
    @DisplayName("Should answer a null action with an error response instead of hanging")
    void shouldAnswerNullAction() {
      String response =
          dispatchAndCapture("{\"requestId\":\"x\",\"nonce\":\"" + CREDENTIALS.getNonce() + "\"}");

      assertTrue(response.contains("Unknown action"));
      assertTrue(response.contains("\"x\""));
    }

    @Test
    @DisplayName("Should execute the handler and send a success response")
    void shouldExecuteHandler() {
      registry.register(new TestActionHandler("testAction"));

      String response = dispatchAndCapture(request("r1", "testAction"));

      assertTrue(response.contains("\"success\":true"));
      assertTrue(response.contains("result"));
    }

    @Test
    @DisplayName("Should answer an unknown action with an error response")
    void shouldAnswerUnknownAction() {
      String response = dispatchAndCapture(request("r1", "nope"));

      assertTrue(response.contains("Unknown action"));
    }

    @Test
    @DisplayName("Should turn a handler exception into an error response without the raw message")
    void shouldTurnHandlerExceptionIntoErrorResponse() {
      registry.register(new CraftforjActionHandler<String>() {
        @Override
        public String getAction() {
          return "boom";
        }

        @Override
        public String handle(JsonObject params) {
          throw new IllegalStateException("secret detail");
        }
      });

      String response = dispatchAndCapture(request("r1", "boom"));

      assertTrue(response.contains("Internal error"));
      assertFalse(response.contains("secret detail"));
    }

    @Test
    @DisplayName("Should send the response once a PendingResult handler completes")
    void shouldSendResponseWhenPendingResultCompletes() {
      PendingResult<Object> pending = new PendingResult<>();
      registry.register(new CraftforjActionHandler<PendingResult<Object>>() {
        @Override
        public String getAction() {
          return "async";
        }

        @Override
        public PendingResult<Object> handle(JsonObject params) {
          return pending;
        }
      });

      Page page = mock(Page.class);
      PageEvent event = mock(PageEvent.class);
      when(event.getData()).thenReturn(Map.of("request", request("r1", "async")));

      registry.dispatch(page, event);
      verify(page, never()).executeJsVoidAsync(anyString());

      pending.complete(Map.of("bell", "data:image/svg+xml,x"));

      ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
      verify(page).executeJsVoidAsync(captor.capture());
      assertTrue(captor.getValue().contains("\"success\":true"));
      assertTrue(captor.getValue().contains("bell"));
    }

    @Test
    @DisplayName("Should send an error response when a PendingResult handler fails")
    void shouldSendErrorWhenPendingResultFails() {
      PendingResult<Object> pending = new PendingResult<>();
      registry.register(new CraftforjActionHandler<PendingResult<Object>>() {
        @Override
        public String getAction() {
          return "asyncFail";
        }

        @Override
        public PendingResult<Object> handle(JsonObject params) {
          return pending;
        }
      });

      Page page = mock(Page.class);
      PageEvent event = mock(PageEvent.class);
      when(event.getData()).thenReturn(Map.of("request", request("r1", "asyncFail")));

      registry.dispatch(page, event);
      pending.completeExceptionally(new IllegalStateException("resolver blew up"));

      ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
      verify(page).executeJsVoidAsync(captor.capture());
      assertTrue(captor.getValue().contains("\"success\":false"));
    }

    @Test
    @DisplayName("Should surface a CraftforjActionException message")
    void shouldSurfaceActionExceptionMessage() {
      registry.register(new CraftforjActionHandler<String>() {
        @Override
        public String getAction() {
          return "fails";
        }

        @Override
        public String handle(JsonObject params) {
          throw new CraftforjActionException("id is required");
        }
      });

      String response = dispatchAndCapture(request("r1", "fails"));

      assertTrue(response.contains("id is required"));
      assertTrue(response.contains("\"success\":false"));
    }
  }

  private static class TestActionHandler implements CraftforjActionHandler<String> {

    private final String action;

    TestActionHandler(String action) {
      this.action = action;
    }

    @Override
    public String getAction() {
      return action;
    }

    @Override
    public String handle(JsonObject params) {
      return "result";
    }
  }
}
