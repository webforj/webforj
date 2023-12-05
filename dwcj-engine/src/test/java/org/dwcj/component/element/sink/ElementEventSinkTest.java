package org.dwcj.component.element.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjWebEvent;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import org.dwcj.component.element.Element;
import org.dwcj.component.element.event.ElementEvent;
import org.dwcj.component.element.event.ElementEventOptions;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.dispatcher.EventListener;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

class ElementEventSinkTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() throws BBjException {
    BBjWebEvent event = Mockito.mock(BBjWebEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();
    Element component = Mockito.mock(Element.class);
    ElementEvent[] dispatchedEvent = new ElementEvent[1];

    String eventType = "click";
    EventListener<ElementEvent> listener = e -> {
      dispatchedEvent[0] = e;
    };
    ElementEventSink sink = new ElementEventSink(component, eventType, dispatcher);
    EventSinkListenerRegistry<ElementEvent> registry =
        new EventSinkListenerRegistry<>(sink, ElementEvent.class);
    registry.addEventListener(listener);

    sink.handleEvent(event);

    assertEquals(eventType, dispatchedEvent[0].getType());
  }

  @Test
  @DisplayName("Test set/remove callback behavior")
  void setRemoveCallbackBehaviour() throws BBjException {
    Map<String, String> items = new HashMap<>() {
      {
        put("x", "event.x");
        put("y", "event.y");
      }
    };
    String code = "component.dispatchEvent(new CustomEvent('custom-event'))";
    String filter = "event.target.isSameNode(component)";

    BBjWebComponent control = mock(BBjWebComponent.class);
    when(control.setCallback(anyString(), any(CustomObject.class), anyString(),
        any(BBjWebEventOptions.class))).thenAnswer(new Answer<Integer>() {
          @Override
          public Integer answer(InvocationOnMock invocation) throws Throwable {
            return new Random().nextInt();
          }
        });
    BBjWebEventOptions controlOptions = spy(BBjWebEventOptions.class);
    when(controlOptions.getCode()).thenReturn(code);
    when(controlOptions.getFilter()).thenReturn(filter);
    when(controlOptions.getItemMap()).thenReturn(items);
    when(control.newEventOptions()).thenReturn(controlOptions);

    Element component = mock(Element.class);
    ElementEventOptions componentOptions = new ElementEventOptions(items, code, filter);
    componentOptions.setDebounce(100);

    String eventType = "click";
    ElementEventSink sink = new ElementEventSink(component, eventType, new EventDispatcher());

    // add the callback
    String firstCallbackId =
        sink.doSetCallback(control, componentOptions, mock(CustomObject.class), "onEvent");
    String secondCallbackId =
        sink.doSetCallback(control, componentOptions, mock(CustomObject.class), "onEvent");

    assertNotEquals(firstCallbackId, secondCallbackId);
    verify(control, times(2)).setCallback(eq(eventType), any(CustomObject.class), anyString(),
        eq(controlOptions));

    verify(control, times(2)).newEventOptions();

    // remove the callback
    sink.doRemoveCallback(control, firstCallbackId);
    verify(control, times(1)).clearCallback(eventType, Integer.parseInt(firstCallbackId));

    sink.doRemoveCallback(control, secondCallbackId);
    verify(control, times(1)).clearCallback(eventType, Integer.parseInt(secondCallbackId));
  }
}
