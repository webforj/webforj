package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.event.BBjExecuteScriptEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.testbed.runtime.RuntimeMock;
import com.webforj.testbed.runtime.ScriptAnswerRegistry;
import com.webforj.testbed.runtime.event.BBjWebEventCallbackRegistry;
import com.webforj.testbed.runtime.event.BBjWebEventOptionsMock;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The mock backing the web component control.
 *
 * <p>
 * Driven by {@code com.webforj.component.element.Element}, which writes and reads html, places
 * children into slots, executes scripts and registers event callbacks under event type names,
 * receiving one identifier per registration and removing them by identifier. Tests fire events
 * through {@link #fireWebEvent(String, Map)} and they reach the component through the registered
 * callbacks exactly as client events do in production.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjWebComponentMock extends BBjControlMock {
  private final AtomicInteger scriptIndexes = new AtomicInteger(0);
  private final BBjWebEventCallbackRegistry webCallbacks =
      new BBjWebEventCallbackRegistry(scriptIndexes);
  private final ScriptAnswerRegistry scriptAnswers;
  private String html = "";

  /**
   * Creates a new web component mock whose scripts nobody answers.
   *
   * @param id the control identifier
   */
  BBjWebComponentMock(int id) {
    this(id, new ScriptAnswerRegistry());
  }

  /**
   * Creates a new web component mock.
   *
   * @param id the control identifier
   * @param scriptAnswers the answers the client gives to scripts
   */
  BBjWebComponentMock(int id, ScriptAnswerRegistry scriptAnswers) {
    super(id);
    this.scriptAnswers = scriptAnswers;
  }

  /**
   * Fires a web component event through every callback registered for the given event type.
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
   * Returns whether a callback is registered for the given web component event type.
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
      case "setHtml" -> {
        html = (String) args[0];
        yield null;
      }
      case "getHtml" -> html;
      case "setSlot" -> null;
      case "newEventOptions" -> new BBjWebEventOptionsMock().getOptions();
      case "executeScript" -> scriptAnswers.nextExecuteJsResult();
      case "executeAsyncScript" -> {
        // The script waits for the test to answer it, after the component stored what it waits for
        // under the index.
        int index = scriptIndexes.incrementAndGet();
        scriptAnswers.await(result -> reportScriptResult(index, result));
        yield index;
      }
      case "setCallback" -> {
        if (args.length >= 3 && args[0] instanceof String eventType
            && args[2] instanceof String methodName) {
          yield webCallbacks.register(eventType, args[1], methodName);
        }
        // Without a browser no element is ever defined, so the defined callback is kept and never
        // fired, and components keep the state they would hand to a client that renders later.
        yield super.handle(proxy, method, args);
      }
      case "clearCallback" -> {
        if (args.length == 1 && args[0] instanceof String eventType) {
          webCallbacks.clear(eventType);
          yield null;
        }
        if (args.length == 2 && args[0] instanceof String eventType
            && args[1] instanceof Integer callbackId) {
          webCallbacks.clear(eventType, callbackId);
          yield null;
        }
        yield super.handle(proxy, method, args);
      }
      default -> super.handle(proxy, method, args);
    };
  }

  private void reportScriptResult(int index, Object result) {
    Map<String, Object> returnValues = new LinkedHashMap<>();
    returnValues.put("getIndex", index);
    returnValues.put("getResult", result);
    getCallbacks().fire(SysGuiEventConstants.ON_EXECUTE_SCRIPT,
        RuntimeMock.createEvent(BBjExecuteScriptEvent.class, returnValues));
  }
}
