package com.webforj.testbed.runtime;

import com.basis.bbj.proxies.BBjBusyIndicator;
import com.basis.bbj.proxies.event.BBjExecuteScriptEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.testbed.runtime.event.BBjControlEventCallbackRegistry;
import com.webforj.testbed.runtime.event.BBjWebEventCallbackRegistry;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The mock backing the web manager of the context under test.
 *
 * <p>
 * Driven by {@code com.webforj.App}, {@code com.webforj.Page} and the page event sink, which carry
 * page level state such as the title, the theme and page attributes through the web manager, hand
 * out the busy indicator and register page level event callbacks. All of this state lives in
 * memory, and page events fire through the registered callbacks with
 * {@link #fireWebEvent(String, Map)}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjWebManagerMock extends RuntimeMock {
  private final Map<String, String> attributes = new ConcurrentHashMap<>();
  private final AtomicInteger scriptIndexes = new AtomicInteger(0);
  private final BBjWebEventCallbackRegistry webCallbacks =
      new BBjWebEventCallbackRegistry(scriptIndexes);
  private final BBjControlEventCallbackRegistry callbacks = new BBjControlEventCallbackRegistry();
  private final BBjBusyIndicatorMock busyIndicator = new BBjBusyIndicatorMock();
  private final ScriptAnswerRegistry scriptAnswers;

  /**
   * Creates a new web manager mock whose scripts nobody answers.
   */
  BBjWebManagerMock() {
    this(new ScriptAnswerRegistry());
  }

  /**
   * Creates a new web manager mock.
   *
   * @param scriptAnswers the answers the client gives to scripts
   */
  BBjWebManagerMock(ScriptAnswerRegistry scriptAnswers) {
    this.scriptAnswers = scriptAnswers;
    setValue("url", "http://localhost/");
  }

  /**
   * Returns the busy indicator mock of this web manager.
   *
   * @return the busy indicator mock
   */
  public BBjBusyIndicatorMock getBusyIndicator() {
    return busyIndicator;
  }

  /**
   * Fires a page event through every callback registered for the given event type.
   *
   * @param eventType the event type name the callbacks were registered under
   * @param payload the event payload
   *
   * @return the number of callbacks invoked
   */
  public int fireWebEvent(String eventType, Map<String, Object> payload) {
    return webCallbacks.fire(eventType, payload);
  }

  /**
   * Returns whether a callback is registered for the given page event type.
   *
   * @param eventType the event type name
   *
   * @return true when at least one callback is registered
   */
  public boolean hasWebCallback(String eventType) {
    return webCallbacks.has(eventType);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setAttribute" -> {
        if (args[1] == null) {
          attributes.remove(args[0]);
        } else {
          attributes.put((String) args[0], (String) args[1]);
        }
        yield null;
      }
      case "getAttribute" -> attributes.get(args[0]);
      case "getBusyIndicator" -> busyIndicator.getProxy(BBjBusyIndicator.class);
      case "setTitle" -> {
        setValue("title", args[0]);
        yield null;
      }
      case "executeScript" -> scriptAnswers.nextExecuteJsResult();
      case "executeAsyncScript" -> {
        // The script waits for the test to answer it, after the page stored what it waits for
        // under the index.
        int index = scriptIndexes.incrementAndGet();
        scriptAnswers.await(result -> reportScriptResult(index, result));
        yield index;
      }
      case "injectStyle", "injectStyleUrl", "injectScript", "injectScriptUrl", "injectLinkUrl",
          "setMeta" -> null;
      case "setCallback" -> {
        if (args.length >= 3 && args[0] instanceof String eventType
            && args[2] instanceof String methodName) {
          yield webCallbacks.register(eventType, args[1], methodName);
        }
        if (args.length == 3 && args[0] instanceof Integer eventType
            && args[2] instanceof String methodName) {
          callbacks.register(eventType, args[1], methodName);
          yield null;
        }
        yield UNHANDLED;
      }
      case "clearCallback" -> {
        if (args.length == 2 && args[0] instanceof String eventType
            && args[1] instanceof Integer callbackId) {
          webCallbacks.clear(eventType, callbackId);
          yield null;
        }
        if (args.length == 1 && args[0] instanceof String eventType) {
          webCallbacks.clear(eventType);
          yield null;
        }
        if (args.length == 1 && args[0] instanceof Integer eventType) {
          callbacks.clear(eventType);
          yield null;
        }
        yield UNHANDLED;
      }
      default -> UNHANDLED;
    };
  }

  private void reportScriptResult(int index, Object result) {
    Map<String, Object> returnValues = new LinkedHashMap<>();
    returnValues.put("getIndex", index);
    returnValues.put("getResult", result);
    callbacks.fire(SysGuiEventConstants.ON_EXECUTE_SCRIPT,
        createEvent(BBjExecuteScriptEvent.class, returnValues));
  }
}
