package com.webforj.testbed.runtime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjBusyIndicator;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.BBjThinClient;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxies.event.BBjExecuteScriptEvent;
import com.basis.bbj.proxies.event.BBjWebEvent;
import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.testbed.runtime.event.BBjWebEventCallbackRegistry;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;

class MockContractsTest {
  private static final String ABSENT_EVENT = "absent";
  private static final String CALLBACK_METHOD = "handleEvent";
  private static final String SCRIPT = "return 1";
  private static final String SCOPED_PROPERTY = "scoped";
  private static final String SPINNER_ATTRIBUTE = "suppress-spinner";
  private static final String UNLOAD_EVENT = "webforj-unload";

  static class PlainMock extends RuntimeMock {
  }

  @Test
  void shouldAnswerNeutralDefaultsForEveryPrimitiveShape() {
    assertEquals(false, RuntimeMock.defaultValue(boolean.class));
    assertEquals(0, RuntimeMock.defaultValue(int.class));
    assertEquals(0L, RuntimeMock.defaultValue(long.class));
    assertEquals(0d, RuntimeMock.defaultValue(double.class));
    assertEquals(0f, RuntimeMock.defaultValue(float.class));
    assertEquals((short) 0, RuntimeMock.defaultValue(short.class));
    assertEquals((byte) 0, RuntimeMock.defaultValue(Byte.class));
    assertEquals((char) 0, RuntimeMock.defaultValue(Character.class));
    assertEquals("", RuntimeMock.defaultValue(String.class));
    assertNotNull(RuntimeMock.defaultValue(BBjVector.class));
    assertNull(RuntimeMock.defaultValue(Object.class));
  }

  @Test
  void shouldResolveBackingMocksOnlyForMockProxies() {
    PlainMock mock = new PlainMock();

    assertTrue(RuntimeMock.getBacking(mock.getProxy(BBjButton.class)).isPresent());
    assertTrue(RuntimeMock.getBacking(null).isEmpty());
    assertTrue(RuntimeMock.getBacking("plain object").isEmpty());
    assertTrue(RuntimeMock.getBacking(Proxy.newProxyInstance(getClass().getClassLoader(),
        new Class<?>[] {Runnable.class}, (proxy, method, args) -> null)).isEmpty());
  }

  @Test
  void shouldKeepUserPropertyCategoriesApart() throws BBjException {
    BBjThinClientMock mock = new BBjThinClientMock();
    BBjThinClient thinClient = mock.getProxy(BBjThinClient.class);

    thinClient.setUserProperties(Map.of("bulk", "1"));
    thinClient.setUserProperties(2L, Map.of(SCOPED_PROPERTY, "2"));
    thinClient.setUserProperties(3L, "attributes", Map.of("attributed", "3"));

    assertEquals("1", thinClient.getUserProperty("bulk"));
    assertEquals("2", thinClient.getUserProperty(2L, SCOPED_PROPERTY));
    assertEquals("3", thinClient.getUserProperty(3L, "attributed"));
    assertEquals(Map.of("bulk", "1"), thinClient.getUserProperties());
    assertEquals(Map.of(SCOPED_PROPERTY, "2"), thinClient.getUserProperties(2L));

    thinClient.setUserProperty(2L, SCOPED_PROPERTY, null);

    assertNull(thinClient.getUserProperty(2L, SCOPED_PROPERTY));

    thinClient.clearUserProperties();

    assertTrue(mock.getUserProperties(0L).isEmpty());
  }

  @Test
  void shouldReportClientTimeZoneChanges() throws BBjException {
    BBjThinClientMock mock = new BBjThinClientMock();
    BBjThinClient thinClient = mock.getProxy(BBjThinClient.class);

    TimeZone berlin = TimeZone.getTimeZone("Europe/Berlin");
    thinClient.setClientTimeZone(berlin);

    assertEquals(berlin, thinClient.getClientTimeZone());

    mock.setClientTimeZone(TimeZone.getTimeZone("UTC"));

    assertEquals("UTC", thinClient.getClientTimeZone().getID());
    assertEquals(System.getProperty("os.name", ""), thinClient.getClientOSName());
    assertEquals(System.getProperty("os.version", ""), thinClient.getClientOSVersion());
  }

  @Test
  void shouldManageWebManagerStateAndCallbacks() throws BBjException {
    BBjWebManagerMock mock = new BBjWebManagerMock();
    BBjWebManager manager = mock.getProxy(BBjWebManager.class);

    assertEquals("http://localhost/", manager.getUrl());

    manager.setAttribute("lang", "en");

    assertEquals("en", manager.getAttribute("lang"));
    assertNull(manager.executeScript(SCRIPT));
    assertTrue(manager.executeAsyncScript(SCRIPT) > 0);

    List<Integer> received = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjWebEvent event) {
        received.add(event.getCallbackID());
      }
    };

    int callbackId = manager.setCallback(UNLOAD_EVENT, handler, CALLBACK_METHOD,
        mock.getProxy(BBjWebManager.class).newEventOptions());

    assertTrue(mock.hasWebCallback(UNLOAD_EVENT));
    assertEquals(1, mock.fireWebEvent(UNLOAD_EVENT, Map.of()));
    assertEquals(List.of(callbackId), received);

    manager.clearCallback(UNLOAD_EVENT, callbackId);

    assertFalse(mock.hasWebCallback(UNLOAD_EVENT));

    int second = manager.setCallback(UNLOAD_EVENT, handler, CALLBACK_METHOD, null);
    assertTrue(second > callbackId);
    manager.clearCallback(UNLOAD_EVENT);

    assertEquals(0, mock.fireWebEvent(UNLOAD_EVENT, Map.of()));
  }

  @Test
  void shouldRoundTripBusyIndicatorState() throws BBjException {
    BBjWebManagerMock manager = new BBjWebManagerMock();
    BBjBusyIndicator indicator = manager.getProxy(BBjWebManager.class).getBusyIndicator();

    indicator.setAttribute(SPINNER_ATTRIBUTE, "true");
    indicator.setText("Working");

    assertEquals("true", indicator.getAttribute(SPINNER_ATTRIBUTE));
    assertEquals("Working", indicator.getText());
    assertEquals(Map.of(SPINNER_ATTRIBUTE, "true"), manager.getBusyIndicator().getAttributes());
  }

  @Test
  void shouldHandOutWindowsAndFindThemById() throws BBjException {
    BBjSysGuiMock mock = new BBjSysGuiMock(new ScriptAnswerRegistry());
    BBjSysGui sysGui = mock.getProxy(BBjSysGui.class);

    assertTrue(sysGui.getWindows().isEmpty());

    BBjWindow window = sysGui.addWindow(null, "", new byte[0]);

    assertNotNull(window);
    assertEquals(1, sysGui.getWindows().size());
    assertEquals(1, mock.getWindowMocks().size());
    assertNotNull(sysGui.getWindow(mock.getWindowMocks().get(0).getId()));
    assertNull(sysGui.getWindow(999));
    assertTrue(sysGui.getAvailableContext() > 0);
  }

  @Test
  void shouldAbsorbEventsWithoutCallbacks() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    BBjAPI api = mock.getApi();

    api.postCustomEvent("nobody-listens", "payload");

    assertFalse(mock.fireTimer("missing"));
    assertNotNull(api.getNamespace("prefix", "name", true));
    assertNotNull(api.getConfig());
    assertNotNull(api.msgbox());
  }

  @Test
  void shouldClearAbsentWebCallbacksSafely() {
    BBjWebEventCallbackRegistry callbacks = new BBjWebEventCallbackRegistry(new AtomicInteger());

    callbacks.clear(ABSENT_EVENT);
    callbacks.clear(ABSENT_EVENT, 5);

    assertFalse(callbacks.has(ABSENT_EVENT));
    assertEquals(0, callbacks.fire(ABSENT_EVENT, Map.of()));
  }

  @Test
  void shouldAnswerPageScriptsWhenAnswered() throws BBjException {
    ScriptAnswerRegistry answers = new ScriptAnswerRegistry();
    BBjWebManagerMock mock = new BBjWebManagerMock(answers);
    BBjWebManager manager = mock.getProxy(BBjWebManager.class);
    List<Object> received = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjExecuteScriptEvent event) {
        received.add(event.getIndex());
        received.add(event.getResult());
      }
    };
    manager.setCallback(SysGuiEventConstants.ON_EXECUTE_SCRIPT, handler, CALLBACK_METHOD);

    int index = manager.executeAsyncScript(SCRIPT);
    answers.answerExecuteJsAsync(null);

    assertEquals(Arrays.asList(index, null), received);

    manager.clearCallback(SysGuiEventConstants.ON_EXECUTE_SCRIPT);
    manager.executeAsyncScript("return 2");
    answers.answerExecuteJsAsync(2);

    assertEquals(2, received.size());
  }
}
