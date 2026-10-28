package com.webforj.testbed.runtime;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjFileOpen;
import com.basis.bbj.proxies.BBjFileSave;
import com.basis.bbj.proxies.BBjInterpreter;
import com.basis.bbj.proxies.BBjMsgBox;
import com.basis.bbj.proxies.BBjNamespace;
import com.basis.bbj.proxies.BBjObjectTable;
import com.basis.bbj.proxies.BBjPrompt;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.BBjThinClient;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxies.event.BBjCustomEvent;
import com.basis.bbj.proxies.event.BBjTimerEvent;
import com.basis.startup.type.BBjException;
import com.webforj.testbed.ShownOptionDialog;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The runtime mock backing the whole context under test.
 *
 * <p>
 * This mock is the root of the runtime object graph. It hands out the window system, the web
 * manager, the client, the object table, the namespaces and the interpreter, all backed by in
 * memory state. Custom events post synchronously to their registered callbacks so that work
 * scheduled from background threads completes deterministically, and timers are stored so that
 * tests fire them explicitly with {@link #fireTimer(Object)}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjApiMock extends RuntimeMock {
  private final ScriptAnswerRegistry scriptAnswers = new ScriptAnswerRegistry();
  private final BBjSysGuiMock sysGui = new BBjSysGuiMock(scriptAnswers);
  private final BBjWebManagerMock webManager = new BBjWebManagerMock(scriptAnswers);
  private final BBjThinClientMock thinClient = new BBjThinClientMock();
  private final BBjObjectTableMock objectTable = new BBjObjectTableMock();
  private final BBjInterpreterMock interpreter = new BBjInterpreterMock();
  private final Map<String, BBjNamespaceMock> namespaces = new ConcurrentHashMap<>();
  private final Map<String, String> stringTable = new ConcurrentHashMap<>();
  private final Map<String, Callback> customEventCallbacks = new ConcurrentHashMap<>();
  private final Map<Object, Callback> timers = new ConcurrentHashMap<>();
  private final Map<String, Object> channelData = new ConcurrentHashMap<>();
  private final OptionDialogAnswerRegistry dialogAnswers = new OptionDialogAnswerRegistry();

  /**
   * Creates a new mock, seeded with an in memory HTTP session accessor.
   *
   * <p>
   * A real deployment always has an HTTP session behind {@link com.webforj.Environment}, so the
   * mock starts with one too.
   * </p>
   */
  public BBjApiMock() {
    channelData.put("session.accessor",
        new TestbedHttpSessionAccessorMock(new TestbedHttpSessionMock()));
  }

  /**
   * Returns the scripted answers to option dialogs and the record of the dialogs shown.
   *
   * @return the dialog answers
   */
  public OptionDialogAnswerRegistry getDialogAnswers() {
    return dialogAnswers;
  }

  /**
   * Returns the answers the client gives to scripts.
   *
   * @return the script answers
   */
  public ScriptAnswerRegistry getScriptAnswers() {
    return scriptAnswers;
  }

  /**
   * Returns the proxy implementing the root runtime interface.
   *
   * @return the root runtime proxy
   */
  BBjAPI getApi() {
    return getProxy(BBjAPI.class);
  }

  /**
   * Returns the window system mock.
   *
   * @return the window system mock
   */
  BBjSysGuiMock getSysGui() {
    return sysGui;
  }

  /**
   * Returns the web manager mock.
   *
   * @return the web manager mock
   */
  BBjWebManagerMock getWebManager() {
    return webManager;
  }

  /**
   * Returns the client mock.
   *
   * @return the client mock
   */
  BBjThinClientMock getThinClient() {
    return thinClient;
  }

  /**
   * Returns the object table mock.
   *
   * @return the object table mock
   */
  BBjObjectTableMock getObjectTable() {
    return objectTable;
  }

  /**
   * Returns the channel data of the context.
   *
   * <p>
   * Tests can seed the channel data before the context starts, for example to expose a session
   * accessor to the application under test.
   * </p>
   *
   * @return the channel data
   */
  Map<String, Object> getChannelData() {
    return channelData;
  }

  /**
   * Fires the timer registered under the given key.
   *
   * @param key the timer key
   *
   * @return true when a timer was registered and fired, false otherwise
   */
  boolean fireTimer(Object key) {
    Callback callback = timers.get(key);
    if (callback == null) {
      return false;
    }

    Map<String, Object> returnValues = new LinkedHashMap<>();
    returnValues.put("getKey", key);
    callback.invoke(createEvent(BBjTimerEvent.class, returnValues));

    return true;
  }

  /**
   * Returns whether a timer is registered under the given key.
   *
   * @param key the timer key
   *
   * @return true when a timer is registered
   */
  boolean hasTimer(Object key) {
    return timers.containsKey(key);
  }

  /**
   * Returns the keys of all registered timers.
   *
   * @return the timer keys
   */
  Set<Object> getTimerKeys() {
    return Set.copyOf(timers.keySet());
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "openSysGui" -> sysGui.getProxy(BBjSysGui.class);
      case "getSysGui" -> sysGui.getProxy(BBjSysGui.class);
      case "getWebManager" -> webManager.getProxy(BBjWebManager.class);
      case "getThinClient" -> thinClient.getProxy(BBjThinClient.class);
      case "getObjectTable" -> objectTable.getProxy(BBjObjectTable.class);
      case "getInterpreter" -> interpreter.getProxy(BBjInterpreter.class);
      case "getGlobalNamespace", "getGroupNamespace" -> namespaceProxy(method.getName());
      case "getNamespace" -> namespaceProxy(args.length == 3 ? args[0] + "." + args[1]
          : firstString(args, method.getName()));
      case "getExistingNamespace", "getNewNamespace" -> namespaceProxy(
          firstString(args, method.getName()));
      case "setStbl" -> {
        stringTable.put((String) args[0], args.length > 1 ? (String) args[1] : "");
        yield args.length > 1 ? args[1] : "";
      }
      case "getStbl" -> {
        String value = stringTable.get(args[0]);
        if (value == null) {
          throw new BBjException("No string table entry for: " + args[0], 0);
        }
        yield value;
      }
      case "setCustomEventCallback" -> {
        customEventCallbacks.put((String) args[0], new Callback(args[1], (String) args[2]));
        yield null;
      }
      case "clearCustomEventCallback" -> {
        customEventCallbacks.remove(args[0]);
        yield null;
      }
      case "postCustomEvent", "postPriorityCustomEvent" -> {
        postCustomEvent((String) args[0], args.length > 1 ? args[1] : null);
        yield null;
      }
      case "createTimer" -> {
        if (args.length == 4) {
          timers.put(args[0], new Callback(args[2], (String) args[3]));
        }
        yield null;
      }
      case "removeTimer" -> {
        timers.remove(args[0]);
        yield null;
      }
      case "getChannelData" -> channelData;
      case "msgbox" -> dialog(ShownOptionDialog.Kind.CONFIRM, BBjMsgBox.class);
      case "prompt" -> dialog(ShownOptionDialog.Kind.INPUT, BBjPrompt.class);
      case "fileopen" -> dialog(ShownOptionDialog.Kind.FILE_CHOOSER, BBjFileOpen.class);
      case "filesave" -> dialog(ShownOptionDialog.Kind.FILE_SAVE, BBjFileSave.class);
      default -> UNHANDLED;
    };
  }

  private void postCustomEvent(String name, Object payload) {
    Callback callback = customEventCallbacks.get(name);
    if (callback == null) {
      return;
    }

    Map<String, Object> returnValues = new LinkedHashMap<>();
    returnValues.put("getName", name);
    returnValues.put("getObject", payload);
    callback.invoke(createEvent(BBjCustomEvent.class, returnValues));
  }

  private Object namespaceProxy(String name) {
    return namespaces.computeIfAbsent(name, BBjNamespaceMock::new).getProxy(BBjNamespace.class);
  }

  private String firstString(Object[] args, String fallback) {
    for (Object arg : args) {
      if (arg instanceof String stringArg) {
        return stringArg;
      }
    }

    return fallback;
  }

  private <T> T dialog(ShownOptionDialog.Kind kind, Class<T> type) {
    return new BBjOptionDialogMock(kind, dialogAnswers).getProxy(type);
  }
}
