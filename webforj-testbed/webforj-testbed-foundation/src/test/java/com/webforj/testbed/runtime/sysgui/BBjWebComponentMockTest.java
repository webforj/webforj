package com.webforj.testbed.runtime.sysgui;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.event.BBjExecuteScriptEvent;
import com.basis.bbj.proxies.event.BBjWebEvent;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.webforj.testbed.runtime.ScriptAnswerRegistry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class BBjWebComponentMockTest {
  private static final String CLICK_EVENT = "click";
  private static final String CALLBACK_METHOD = "handleEvent";
  private static final String INPUT_EVENT = "input";
  private static final String SCRIPT = "return 1";

  @Test
  void shouldStoreAndFireWebCallbacks() throws BBjException {
    BBjWebComponentMock mock = new BBjWebComponentMock(1);
    BBjWebComponent control = mock.getProxy(BBjWebComponent.class);

    List<Object> received = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjWebEvent event) {
        received.add(event.getEventMap().get("x"));
      }
    };

    BBjWebEventOptions options = control.newEventOptions();
    assertNotNull(options);

    int callbackId = control.setCallback(CLICK_EVENT, handler, CALLBACK_METHOD, options);

    assertTrue(mock.hasWebCallback(CLICK_EVENT));
    assertEquals(1, mock.fireWebEvent(CLICK_EVENT, Map.of("x", 11)));

    control.clearCallback(CLICK_EVENT, callbackId);

    assertEquals(List.of(11), received);
    assertFalse(mock.hasWebCallback(CLICK_EVENT));
    assertEquals(0, mock.fireWebEvent(CLICK_EVENT, Map.of("x", 11)));
  }

  @Test
  void shouldClearAllWebCallbacksForType() throws BBjException {
    BBjWebComponentMock mock = new BBjWebComponentMock(1);
    BBjWebComponent control = mock.getProxy(BBjWebComponent.class);
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjWebEvent event) {} // NOSONAR
    };

    control.setCallback(INPUT_EVENT, handler, CALLBACK_METHOD);
    control.clearCallback(INPUT_EVENT);

    assertFalse(mock.hasWebCallback(INPUT_EVENT));
  }

  @Test
  void shouldDeliverCallbackIdentifiersToEachRegistration() throws BBjException {
    BBjWebComponentMock mock = new BBjWebComponentMock(1);
    BBjWebComponent control = mock.getProxy(BBjWebComponent.class);

    List<Integer> receivedIds = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjWebEvent event) {
        receivedIds.add(event.getCallbackID());
      }
    };

    int first = control.setCallback(CLICK_EVENT, handler, CALLBACK_METHOD);
    int second = control.setCallback(CLICK_EVENT, handler, CALLBACK_METHOD);

    assertEquals(2, mock.fireWebEvent(CLICK_EVENT, Map.of()));
    assertTrue(receivedIds.contains(first));
    assertTrue(receivedIds.contains(second));
  }

  @Test
  void shouldExecuteScriptsWithoutEffect() throws BBjException {
    BBjWebComponentMock mock = new BBjWebComponentMock(1);
    BBjWebComponent control = mock.getProxy(BBjWebComponent.class);

    assertEquals(null, control.executeScript(SCRIPT));
    assertTrue(control.executeAsyncScript(SCRIPT) > 0);
  }

  @Test
  void shouldKeepBaseControlSurface() throws BBjException {
    BBjWebComponentMock mock = new BBjWebComponentMock(1);
    BBjWebComponent control = mock.getProxy(BBjWebComponent.class);

    control.setAttribute("id", "chip");

    assertEquals("chip", control.getAttribute("id"));
    assertTrue(control.isVisible());
  }

  @Test
  void shouldAnswerAsyncScriptsWhenAnswered() throws BBjException {
    ScriptAnswerRegistry answers = new ScriptAnswerRegistry();
    BBjWebComponentMock mock = new BBjWebComponentMock(1, answers);
    BBjWebComponent control = mock.getProxy(BBjWebComponent.class);
    List<Object> received = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjExecuteScriptEvent event) {
        received.add(event.getIndex());
        received.add(event.getResult());
      }
    };
    control.setCallback(SysGuiEventConstants.ON_EXECUTE_SCRIPT, handler, CALLBACK_METHOD);

    int index = control.executeAsyncScript(SCRIPT);

    assertTrue(received.isEmpty());

    answers.answerExecuteJsAsync(7);

    assertEquals(Arrays.asList(index, 7), received);
  }
}
