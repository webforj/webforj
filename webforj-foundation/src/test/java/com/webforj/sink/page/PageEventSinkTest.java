package com.webforj.sink.page;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxies.event.BBjWebEvent;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.mockito.stubbing.Answer;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class PageEventSinkTest {
  EventDispatcher dispatcher;
  Page page;
  Environment environment;
  BBjAPI api;
  BBjWebManager webManager;
  PageEventSink sink;

  @BeforeEach
  void setUp() throws BBjException {
    dispatcher = new EventDispatcher();
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    webManager = mock(BBjWebManager.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(api.getWebManager()).thenReturn(webManager);

    page = spy(Page.class);
    sink = spy(new PageEventSink(page, "click", dispatcher));
  }

  @Test
  void shouldSetCallback() throws BBjException {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);

      BBjWebEventOptions optionsMock = mock(BBjWebEventOptions.class);
      when(webManager.newEventOptions()).thenReturn(optionsMock);

      sink.setCallback(new PageEventOptions());
      verify(sink.getBbjWebManager(), times(1)).setCallback(eq("click"), any(), eq("handleEvent"),
          eq(optionsMock));
    }
  }

  @Test
  void shouldRemoveCallback() throws BBjException {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);

      sink.removeCallback("5");
      verify(webManager, times(1)).clearCallback("click", 5);
    }
  }

  @Test
  void shouldProcessPayload() throws BBjException {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);

      BBjWebEventOptions optionsMock = mock(BBjWebEventOptions.class);
      when(webManager.newEventOptions()).thenReturn(optionsMock);

      BBjWebEvent event = Mockito.mock(BBjWebEvent.class);
      PageEvent[] dispatchedEvent = new PageEvent[1];

      String eventType = "click";
      EventListener<PageEvent> listener = e -> {
        dispatchedEvent[0] = e;
      };

      PageEventSinkRegistry registry = new PageEventSinkRegistry(sink, PageEvent.class);
      registry.addEventListener(listener, new PageEventOptions());

      sink.handleEvent(event);

      assertEquals(eventType, dispatchedEvent[0].getType());
    }
  }

  @Test
  void setRemoveCallbackBehaviour() throws BBjException {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);

      Map<String, String> items = new HashMap<>() {
        {
          put("x", "event.x");
          put("y", "event.y");
        }
      };
      String code = "component.dispatchEvent(new CustomEvent('custom-event'))";
      String filter = "event.target.isSameNode(component)";

      when(webManager.setCallback(anyString(), any(), anyString(), any(BBjWebEventOptions.class)))
          .thenAnswer(new Answer<Integer>() {
            @Override
            public Integer answer(InvocationOnMock invocation) throws Throwable {
              return new Random().nextInt();
            }
          });
      BBjWebEventOptions managerOptions = mock(BBjWebEventOptions.class);
      when(managerOptions.getCode()).thenReturn(code);
      when(managerOptions.getFilter()).thenReturn(filter);
      when(managerOptions.getItemMap()).thenReturn(items);
      when(webManager.newEventOptions()).thenReturn(managerOptions);

      PageEventOptions pageOptions = new PageEventOptions(items, code, filter);
      pageOptions.setDebounce(100);

      String eventType = "click";

      // add the callback
      String firstCallbackId = sink.doSetCallback(webManager, pageOptions, sink, "handleEvent");
      String secondCallbackId = sink.doSetCallback(webManager, pageOptions, sink, "handleEvent");

      assertNotEquals(firstCallbackId, secondCallbackId);
      verify(webManager, times(2)).setCallback(eq(eventType), any(), anyString(),
          eq(managerOptions));

      verify(webManager, times(2)).newEventOptions();

      // remove the callback
      sink.doRemoveCallback(webManager, firstCallbackId);
      verify(webManager, times(1)).clearCallback(eventType, Integer.parseInt(firstCallbackId));

      sink.doRemoveCallback(webManager, secondCallbackId);
      verify(webManager, times(1)).clearCallback(eventType, Integer.parseInt(secondCallbackId));
    }
  }
}
