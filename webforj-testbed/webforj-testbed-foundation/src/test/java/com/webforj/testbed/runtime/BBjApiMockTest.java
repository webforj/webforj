package com.webforj.testbed.runtime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjNamespace;
import com.basis.bbj.proxies.event.BBjCustomEvent;
import com.basis.bbj.proxies.event.BBjTimerEvent;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import java.util.ArrayList;
import java.util.List;
import java.util.TimeZone;
import org.junit.jupiter.api.Test;

class BBjApiMockTest {
  private static final String CATEGORY_PROPERTY = "categorized";
  private static final String CALLBACK_METHOD = "handleEvent";
  private static final String EVENT_NAME = "my-event";
  private static final String NAMESPACE_NAME = "shared";
  private static final String STORED_VALUE = "value";

  @Test
  void shouldHandOutStableCollaborators() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    BBjAPI api = mock.getApi();

    assertSame(api.openSysGui("X0"), api.getSysGui());
    assertNotNull(api.getWebManager());
    assertNotNull(api.getThinClient());
    assertNotNull(api.getObjectTable());
    assertNotNull(api.getInterpreter());
    assertNotNull(api.getGlobalNamespace());
  }

  @Test
  void shouldKeepStringTableEntries() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    BBjAPI api = mock.getApi();

    api.setStbl("!KEY", STORED_VALUE);

    assertEquals(STORED_VALUE, api.getStbl("!KEY"));
    assertThrows(BBjException.class, () -> api.getStbl("!MISSING"));
  }

  @Test
  void shouldDispatchCustomEventsSynchronously() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    BBjAPI api = mock.getApi();

    List<Object> received = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjCustomEvent event) {
        received.add(event.getObject());
      }
    };

    api.setCustomEventCallback(EVENT_NAME, handler, CALLBACK_METHOD);
    api.postCustomEvent(EVENT_NAME, "payload");
    api.postPriorityCustomEvent(EVENT_NAME, "priority");

    assertEquals(List.of("payload", "priority"), received);

    api.clearCustomEventCallback(EVENT_NAME);
    api.postCustomEvent(EVENT_NAME, "ignored");

    assertEquals(2, received.size());
  }

  @Test
  void shouldStoreAndFireTimers() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    BBjAPI api = mock.getApi();

    List<Object> ticks = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjTimerEvent event) {
        ticks.add(event.getKey());
      }
    };

    api.createTimer("tick", BasisNumber.createBasisNumber(1), handler, CALLBACK_METHOD);

    assertTrue(mock.hasTimer("tick"));
    assertEquals(1, mock.getTimerKeys().size());
    assertTrue(mock.fireTimer("tick"));
    assertEquals(List.of("tick"), ticks);

    api.removeTimer("tick");

    assertFalse(mock.hasTimer("tick"));
    assertFalse(mock.fireTimer("tick"));
  }

  @Test
  void shouldKeepNamespaceState() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    BBjAPI api = mock.getApi();

    api.getNewNamespace(NAMESPACE_NAME).setValue("count", 5);

    assertEquals(5, api.getExistingNamespace(NAMESPACE_NAME).getValue("count"));
    BBjNamespace namespace = api.getExistingNamespace(NAMESPACE_NAME);
    assertThrows(BBjException.class, () -> namespace.getValue("missing"));
    assertEquals(1, api.getExistingNamespace(NAMESPACE_NAME).getKeys().size());
    assertEquals(NAMESPACE_NAME, api.getExistingNamespace(NAMESPACE_NAME).getName());
    assertNotNull(api.getNamespace("prefix", "scoped", true));
  }

  @Test
  void shouldKeepObjectTableState() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    BBjAPI api = mock.getApi();

    api.getObjectTable().put("key", STORED_VALUE);

    assertEquals(STORED_VALUE, api.getObjectTable().get("key"));
    assertEquals(1, api.getObjectTable().size());
    assertThrows(BBjException.class, () -> api.getObjectTable().get("missing"));

    api.getObjectTable().remove("key");
    assertEquals(0, api.getObjectTable().size());

    api.getObjectTable().put("key", STORED_VALUE);
    api.getObjectTable().clear();
    assertEquals(0, mock.getObjectTable().getEntries().size());
  }

  @Test
  void shouldExposeChannelData() throws BBjException {
    BBjApiMock mock = new BBjApiMock();

    mock.getChannelData().put("session.accessor", "accessor");

    assertEquals("accessor", mock.getApi().getChannelData().get("session.accessor"));
  }

  @Test
  void shouldReportDeterministicClientFacts() throws BBjException {
    BBjApiMock mock = new BBjApiMock();

    assertEquals("en-US", mock.getApi().getThinClient().getClientLocale());
    assertEquals(List.of("en-US"), mock.getApi().getThinClient().getClientLocales());
    assertEquals(TimeZone.getTimeZone("UTC"), mock.getApi().getThinClient().getClientTimeZone());
    assertEquals("127.0.0.1", mock.getApi().getThinClient().getClientIPAddress());

    mock.getThinClient().setClientLocale("de-DE");

    assertEquals("de-DE", mock.getApi().getThinClient().getClientLocale());
  }

  @Test
  void shouldKeepUserPropertiesPerCategory() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    var thinClient = mock.getApi().getThinClient();

    thinClient.setUserProperty("plain", "1");
    thinClient.setUserProperty(2L, CATEGORY_PROPERTY, "2");

    assertEquals("1", thinClient.getUserProperty("plain"));
    assertEquals("2", thinClient.getUserProperty(2L, CATEGORY_PROPERTY));
    assertEquals("2", mock.getThinClient().getUserProperties(2L).get(CATEGORY_PROPERTY));

    thinClient.clearUserProperties(2L);

    assertTrue(mock.getThinClient().getUserProperties(2L).isEmpty());
  }

  @Test
  void shouldRunInterpreterWorkSynchronously() throws Exception {
    BBjApiMock mock = new BBjApiMock();

    assertEquals("result",
        mock.getApi().getInterpreter().executeInInterpreterContext(() -> "result").get());
    assertThrows(BBjException.class,
        () -> mock.getApi().getInterpreter().executeInInterpreterContext(() -> {
          throw new IllegalStateException("boom");
        }));
  }

  @Test
  void shouldDeliverCustomEvents() throws BBjException {
    BBjApiMock mock = new BBjApiMock();
    BBjAPI api = mock.getApi();
    List<String> order = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjCustomEvent event) {
        order.add("custom");
      }
    };
    api.setCustomEventCallback(EVENT_NAME, handler, CALLBACK_METHOD);

    api.postCustomEvent(EVENT_NAME, null);

    assertEquals(List.of("custom"), order);
  }
}
