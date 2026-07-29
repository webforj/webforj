package com.webforj.devtools.craftforj.action;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.devtools.craftforj.security.ChannelCredentials;
import com.webforj.event.page.PageEvent;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Registry and dispatcher for craftforJ action handlers.
 *
 * <p>
 * This class maintains a registry of {@link CraftforjActionHandler} instances and dispatches
 * incoming requests to the appropriate handler.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CraftforjActionRegistry {

  private static final Logger LOGGER = System.getLogger(CraftforjActionRegistry.class.getName());
  private static final Gson GSON = new GsonBuilder().serializeNulls().create();
  private final Map<String, CraftforjActionHandler<?>> handlers = new ConcurrentHashMap<>();
  private final ChannelCredentials credentials;

  /**
   * Creates a registry bound to the given channel credentials.
   *
   * @param credentials the credentials every request has to present
   */
  public CraftforjActionRegistry(ChannelCredentials credentials) {
    this.credentials = credentials;
  }

  /**
   * Registers an action handler.
   *
   * @param handler the handler to register
   * @throws IllegalArgumentException if a handler for the same action is already registered
   */
  public void register(CraftforjActionHandler<?> handler) {
    String action = handler.getAction();
    if (handlers.putIfAbsent(action, handler) != null) {
      throw new IllegalArgumentException("Handler already registered for action: " + action);
    }
  }

  /**
   * Unregisters an action handler.
   *
   * @param action the action name to unregister
   * @return true if a handler was removed, false otherwise
   */
  public boolean unregister(String action) {
    return handlers.remove(action) != null;
  }

  /**
   * Checks if a handler is registered for the given action.
   *
   * @param action the action name
   * @return true if a handler is registered
   */
  boolean hasHandler(String action) {
    return handlers.containsKey(action);
  }

  /**
   * Dispatches a page event to the appropriate handler.
   *
   * @param page the current page
   * @param event the page event containing request JSON
   */
  public void dispatch(Page page, PageEvent event) {
    String requestJson = (String) event.getData().get("request");
    CraftforjRequest request = CraftforjRequest.fromJson(requestJson);
    if (request == null) {
      sendResponse(page,
          CraftforjResponse.error(null, "Invalid request: missing or malformed JSON"));

      return;
    }

    // A request without the nonce the server handed to this page did not come through the
    // channel, so it gets no answer at all, not even an error a caller could probe with.
    if (!credentials.matches(request.getNonce())) {
      LOGGER.log(Level.WARNING, "Rejected a craftforJ request without a valid channel nonce: {0}",
          request.getAction());

      return;
    }

    CraftforjResponse response = executeHandler(page, request);
    if (response != null) {
      sendResponse(page, response);
    }
  }

  private CraftforjResponse executeHandler(Page page, CraftforjRequest request) {
    String action = request.getAction();
    boolean debug = LOGGER.isLoggable(Level.DEBUG);

    CraftforjActionHandler<?> handler = action == null ? null : handlers.get(action);
    if (handler == null) {
      LOGGER.log(Level.WARNING, "Unknown action: {0}", action);
      return CraftforjResponse.error(request.getRequestId(), "Unknown action: " + action);
    }

    if (debug) {
      LOGGER.log(Level.DEBUG, ">>> {0} params={1}", action, request.getParams());
    }
    long start = System.currentTimeMillis();

    try {
      Object result = handler.handle(request.getParams());

      // Handlers that need a browser round trip return a PendingResult; the response is sent
      // once it completes, correlated by requestId like any other response.
      if (result instanceof PendingResult<?> pending) {
        pending.thenAccept(value -> {
          if (debug) {
            long duration = System.currentTimeMillis() - start;
            LOGGER.log(Level.DEBUG, "<<< {0} ({1}ms) async response={2}", action, duration, value);
          }
          sendResponse(page, CraftforjResponse.success(request.getRequestId(), value));
        }).exceptionally(ex -> {
          LOGGER.log(Level.WARNING, "<<< {0} FAILED async: {1}", action, ex.getMessage());
          sendResponse(page, CraftforjResponse.error(request.getRequestId(), ex.getMessage()));

          return null;
        });

        return null;
      }

      long duration = System.currentTimeMillis() - start;
      if (debug) {
        LOGGER.log(Level.DEBUG, "<<< {0} ({1}ms) response={2}", action, duration, result);
      }

      return CraftforjResponse.success(request.getRequestId(), result);
    } catch (CraftforjActionException e) {
      LOGGER.log(Level.WARNING, "<<< {0} FAILED: {1}", action, e.getMessage());

      return CraftforjResponse.error(request.getRequestId(), e.getMessage());
    } catch (Exception e) {
      LOGGER.log(Level.ERROR, "Action " + action + " failed", e);

      return CraftforjResponse.error(request.getRequestId(), "Internal error");
    }
  }

  private void sendResponse(Page page, CraftforjResponse response) {
    String responseJson = GSON.toJson(response);
    String sink = "window." + responseSinkName(credentials);
    page.executeJsVoidAsync(sink + " && " + sink + "(" + responseJson + ")");
  }

  /**
   * Builds the name of the function the client installs to receive responses.
   *
   * <p>
   * This prefix is the one seam that crosses the language boundary. The client builds the same name
   * from {@code RESPONSE_SINK_PREFIX} in the shared channel contract (packages/core
   * src/channel/contract.ts), so the two must agree.
   * </p>
   *
   * @param credentials the channel credentials
   * @return the sink function name
   */
  public static String responseSinkName(ChannelCredentials credentials) {
    return "__webforjDevToolsResponse_" + credentials.getSinkId();
  }
}
