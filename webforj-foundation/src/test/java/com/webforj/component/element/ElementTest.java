package com.webforj.component.element;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjWebEvent;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.startup.type.BBjException;
import com.webforj.PendingResult;
import com.webforj.component.Component;
import com.webforj.component.ComponentMock;
import com.webforj.component.ComponentUtil;
import com.webforj.component.DwcComponentMock;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.element.event.ElementDefinedEvent;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.element.sink.ElementEventSink;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ElementTest {

  @Mock
  BBjWebComponent control;

  @InjectMocks
  Element component = new Element("div");

  @Test
  @DisplayName("throw NullPointerException when tag name is null")
  void throwNullPointerExceptionWhenTagNameIsNull() {
    assertThrows(NullPointerException.class, () -> new Element(null));
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    @DisplayName("Should add any component as child")
    void shouldAddAnyComponentAsChild() throws BBjException {
      DwcComponentMock child = new DwcComponentMock() {
        {
          setControl(control);
        }
      };

      component.add(child);
      assertEquals(1, component.getComponentCount());
      component.onAttach();
      verify(control, times(1)).setSlot("", control);
    }

    @Test
    @DisplayName("Should add any component as child and assign slot")
    void shouldAddAnyComponentAsChildAndAssignSlot() throws BBjException {
      DwcComponentMock first = new DwcComponentMock() {
        {
          setControl(control);
        }
      };

      DwcComponentMock second = new DwcComponentMock() {
        {
          setControl(control);
        }
      };

      component.add("first", first);
      component.add("second", second);
      assertEquals(2, component.getComponentCount());
      component.onAttach();
      verify(control, times(1)).setSlot("first", control);
      verify(control, times(1)).setSlot("second", control);
    }

    @Test
    @DisplayName("Add Throws IllegalArgumentException for Component Without Control")
    @SuppressWarnings("squid:S5778")
    void addThrowsExceptionIfNoControl() {
      ComponentMock child = new ComponentMock();

      assertThrows(IllegalArgumentException.class, () -> {
        component.add(child);
        component.onAttach();
      });
    }

    @Test
    void shouldRemoveComponentFromSlot() {
      DwcComponentMock child1 = new DwcComponentMock() {
        {
          setControl(control);
        }
      };

      DwcComponentMock child2 = new DwcComponentMock() {
        {
          setControl(control);
        }
      };

      component.add("first", child1);
      component.add("first", child2);

      component.onAttach();
      assertEquals(2, component.getComponentCount());

      component.remove(child1);
      assertEquals(1, component.getComponentCount());
      assertFalse(component.hasComponent(child1));
      assertNull(component.findComponentSlot(child1));
      assertEquals(child2, component.getFirstComponentInSlot("first"));
    }
  }

  @Nested
  @DisplayName("HTML API")
  class HtmlApi {

    @Test
    @DisplayName("Setting/getting html when control is null")
    void settingGettingHtmlWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setHtml("html");
      assertEquals("html", component.getHtml());

      verify(control, times(0)).setHtml(anyString());
      verify(control, times(0)).getHtml();
    }

    @Test
    @DisplayName("Setting/getting html when control is not null")
    void settingGettingHtmlWhenControlIsNotNull() throws BBjException {
      when(control.getHtml()).thenReturn("html");
      component.setHtml("html");

      assertEquals("html", component.getHtml());

      verify(control, times(1)).setHtml(anyString());
      verify(control, times(1)).getHtml();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void shouldThrowDwcjRuntimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setHtml(anyString());
      assertThrows(WebforjRuntimeException.class, () -> component.setHtml("html"));

      doThrow(BBjException.class).when(control).getHtml();
      assertThrows(WebforjRuntimeException.class, () -> component.getHtml());
    }

    @Test
    @DisplayName("onAttach will re-apply html changes")
    void onAttachWillReapplyHtmlChanges() throws BBjException, IllegalAccessException {
      component.setHtml("html");
      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();
      verify(control, times(2)).setHtml(anyString());
    }
  }

  @Nested
  @DisplayName("DisableOnClick API")
  class DisableOnClickApi {

    @Test
    @DisplayName("Setting/getting disableOnClick when control is null")
    void settingGettingDisableOnClickWhenControlIsNull()
        throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setDisableOnClick(true);
      assertTrue(component.isDisableOnClick());

      verify(control, times(0)).setDisableOnClick(true);
      verify(control, times(0)).getDisableOnClick();
    }

    @Test
    @DisplayName("Setting/getting disableOnClick when control is not null")
    void settingGettingDisableOnClickWhenControlIsNotNull() throws BBjException {
      when(control.getDisableOnClick()).thenReturn(true);
      component.setDisableOnClick(true);

      assertTrue(component.isDisableOnClick());

      verify(control, times(1)).setDisableOnClick(true);
      verify(control, times(1)).getDisableOnClick();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void shouldThrowDwcjRuntimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setDisableOnClick(true);
      assertThrows(WebforjRuntimeException.class, () -> component.setDisableOnClick(true));

      doThrow(BBjException.class).when(control).getDisableOnClick();
      assertThrows(WebforjRuntimeException.class, () -> component.isDisableOnClick());
    }

    @Test
    @DisplayName("onAttach will re-apply disableOnClick changes")
    void onAttachWillReapplyDisableOnClickChanges() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setDisableOnClick(true);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setDisableOnClick(true);
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    @DisplayName("Adding events with different params will return different ListenerRegistration")
    void addingEventWithDifferentParams() throws BBjException {
      when(control.newEventOptions()).thenReturn(mock(BBjWebEventOptions.class));

      String type = "click";
      EventListener<ElementEvent> listener = event -> {
      };
      ElementEventOptions options1 = new ElementEventOptions(null, "code", "filter");
      ElementEventOptions options2 = new ElementEventOptions(null, "code", null);

      ListenerRegistration<ElementEvent> r1 = component.addEventListener(type, listener, options1);
      ListenerRegistration<ElementEvent> r2 = component.addEventListener(type, listener, options2);
      ListenerRegistration<ElementEvent> r3 = component.addEventListener(type, listener, options1);

      assertNotSame(r1, r2);
      assertNotSame(r1, r3);
      assertNotSame(r2, r3);
    }

    @Test
    @DisplayName("should deliver a handled event to every listener of its type")
    void shouldDeliverHandledEventToEveryListenerOfItsType() throws BBjException {
      when(control.newEventOptions()).thenReturn(mock(BBjWebEventOptions.class));
      when(control.setCallback(anyString(), any(), anyString(), any(BBjWebEventOptions.class)))
          .thenReturn(1, 2, 3);
      List<ElementEvent> first = new ArrayList<>();
      List<ElementEvent> second = new ArrayList<>();
      List<ElementEvent> other = new ArrayList<>();
      component.addEventListener("progress", first::add);
      component.addEventListener("progress", second::add);
      component.addEventListener("other", other::add);
      Map<String, Object> data = Map.of("percent", 50);

      component.handleEvent("progress", data);

      assertEquals(1, first.size());
      assertEquals(1, second.size());
      assertTrue(other.isEmpty());
      assertEquals("progress", first.get(0).getType());
      assertEquals(data, first.get(0).getEventMap());
      assertSame(component, second.get(0).getComponent());
    }

    @Test
    @DisplayName("should ignore a handled event without listeners of its type")
    void shouldIgnoreHandledEventWithoutListeners() {
      Map<String, Object> data = Map.of();

      assertDoesNotThrow(() -> component.handleEvent("progress", data));
    }

    @Test
    @DisplayName("should deliver a handled event before the element is attached")
    void shouldDeliverHandledEventBeforeAttach() {
      Element element = new Element("div");
      List<ElementEvent> received = new ArrayList<>();
      element.addEventListener("progress", received::add);
      Map<String, Object> data = Map.of("percent", 50);

      element.handleEvent("progress", data);

      assertEquals(1, received.size());
      assertEquals(data, received.get(0).getData());
    }

    @Test
    @DisplayName("should deliver a handled event with the options of the listener")
    void shouldDeliverHandledEventWithListenerOptions() throws BBjException {
      when(control.newEventOptions()).thenReturn(mock(BBjWebEventOptions.class));
      when(control.setCallback(anyString(), any(), anyString(), any(BBjWebEventOptions.class)))
          .thenReturn(1);
      List<ElementEvent> received = new ArrayList<>();
      component.addEventListener("progress", received::add,
          new ElementEventOptions().setCode("event.preventDefault()"));

      component.handleEvent("progress", Map.of());

      assertEquals("event.preventDefault()", received.get(0).getOptions().getCode());
    }

    @Test
    @DisplayName("should deliver a handled event without data as an event with empty data")
    void shouldDeliverHandledEventWithoutDataAsEventWithEmptyData() {
      Element element = new Element("div");
      List<ElementEvent> received = new ArrayList<>();
      element.addEventListener("progress", received::add);

      element.handleEvent("progress", null);

      assertEquals(Map.of(), received.get(0).getData());
    }

    @Test
    @DisplayName("should route a fired element event to the listeners of its type")
    void shouldRouteFiredElementEventToListenersOfItsType() throws BBjException {
      when(control.newEventOptions()).thenReturn(mock(BBjWebEventOptions.class));
      when(control.setCallback(anyString(), any(), anyString(), any(BBjWebEventOptions.class)))
          .thenReturn(1, 2);
      List<ElementEvent> clicks = new ArrayList<>();
      List<ElementEvent> other = new ArrayList<>();
      component.addEventListener("click", clicks::add);
      component.addEventListener("other", other::add);
      ElementEvent event = new ElementEvent(component, Map.of("x", 1), "click", 0, null);

      ComponentUtil.fireComponentEvent(component, event);

      assertEquals(1, clicks.size());
      assertEquals(Map.of("x", 1), clicks.get(0).getData());
      assertTrue(other.isEmpty());
    }

    @Test
    @DisplayName("should deliver a fired event of another class to the element dispatcher")
    void shouldDeliverFiredEventOfAnotherClassToElementDispatcher() {
      Element element = new Element("div");
      ElementDefinedEvent event = new ElementDefinedEvent(element, Map.of());

      ComponentUtil.fireComponentEvent(element, event);

      assertTrue(element.isDefined());
    }

    @Test
    @DisplayName("should reject a handled event without a type")
    void shouldRejectHandledEventWithoutType() {
      String type = null;
      Map<String, Object> data = Map.of();

      NullPointerException exception =
          assertThrows(NullPointerException.class, () -> component.handleEvent(type, data));
      assertEquals("The event type must not be null", exception.getMessage());
    }

    @Test
    @DisplayName("should deliver a client event only to the listener of its callback id")
    void shouldDeliverClientEventOnlyToListenerOfItsCallbackId() throws BBjException {
      when(control.newEventOptions()).thenReturn(mock(BBjWebEventOptions.class));
      when(control.setCallback(anyString(), any(), anyString(), any(BBjWebEventOptions.class)))
          .thenReturn(1, 2);
      List<ElementEvent> first = new ArrayList<>();
      List<ElementEvent> second = new ArrayList<>();
      component.addEventListener("progress", first::add);
      component.addEventListener("progress", second::add);
      ElementEventSink sink = findSink("progress");
      BBjWebEvent event = createClientEvent(1);

      sink.handleEvent(event);

      assertEquals(1, first.size());
      assertTrue(second.isEmpty());
    }

    @Test
    @DisplayName("should not deliver a client event to the listeners of another type")
    void shouldNotDeliverClientEventToListenersOfAnotherType() throws BBjException {
      when(control.newEventOptions()).thenReturn(mock(BBjWebEventOptions.class));
      when(control.setCallback(anyString(), any(), anyString(), any(BBjWebEventOptions.class)))
          .thenReturn(1, 2);
      List<ElementEvent> progress = new ArrayList<>();
      List<ElementEvent> other = new ArrayList<>();
      component.addEventListener("progress", progress::add);
      component.addEventListener("other", other::add);
      ElementEventSink sink = findSink("other");
      BBjWebEvent event = createClientEvent(2);

      sink.handleEvent(event);

      assertTrue(progress.isEmpty());
      assertEquals(1, other.size());
    }

    @Test
    @DisplayName("should ignore a client event with a callback id of no listener")
    void shouldIgnoreClientEventWithCallbackIdOfNoListener() throws BBjException {
      when(control.newEventOptions()).thenReturn(mock(BBjWebEventOptions.class));
      when(control.setCallback(anyString(), any(), anyString(), any(BBjWebEventOptions.class)))
          .thenReturn(1, 2);
      List<ElementEvent> progress = new ArrayList<>();
      List<ElementEvent> other = new ArrayList<>();
      component.addEventListener("progress", progress::add);
      component.addEventListener("other", other::add);
      ElementEventSink sink = findSink("progress");
      BBjWebEvent event = createClientEvent(3);

      sink.handleEvent(event);

      assertTrue(progress.isEmpty());
      assertTrue(other.isEmpty());
    }

    private ElementEventSink findSink(String type) throws BBjException {
      ArgumentCaptor<Object> handler = ArgumentCaptor.forClass(Object.class);
      verify(control, atLeastOnce()).setCallback(eq(type), handler.capture(), anyString(),
          any(BBjWebEventOptions.class));

      return (ElementEventSink) handler.getValue();
    }

    private BBjWebEvent createClientEvent(int callbackId) {
      BBjWebEvent event = mock(BBjWebEvent.class);
      when(event.getCallbackID()).thenReturn(callbackId);
      when(event.getEventMap()).thenReturn(Map.of());

      return event;
    }
  }

  @Nested
  @DisplayName("whenDefined Method")
  class WhenDefinedMethod {

    @Test
    @DisplayName("should immediately complete if element is already defined")
    void shouldCompleteImmediatelyIfDefined() {
      Element spy = spy(component);
      when(spy.isDefined()).thenReturn(true);

      PendingResult<Element> result = spy.whenDefined();
      assertTrue(result.isDone());
    }

    @Test
    @DisplayName("should complete when element becomes defined")
    void shouldCompleteWhenElementBecomesDefined() {
      Element spy = spy(component);
      when(spy.isDefined()).thenReturn(false);

      PendingResult<Element> result = spy.whenDefined();
      assertFalse(result.isDone());

      spy.onAttach();
      spy.onDefinedEventEvent(mock(ElementDefinedEvent.class));

      assertTrue(result.isDone());
    }
  }

  @Nested
  @DisplayName("buildCallJsFunctionScript Method")
  class BuildCallJsFunctionScriptMethod {

    @Test
    @DisplayName("should not accept empty function name")
    void shouldNotAcceptEmptyFunctionName() {
      assertThrows(IllegalArgumentException.class,
          () -> component.buildCallJsFunctionScript("", true));
    }

    @Test
    @DisplayName("function name must not start with dot")
    void functionNameMustNotStartWithDot() {
      assertThrows(IllegalArgumentException.class,
          () -> component.buildCallJsFunctionScript(".test", true));
    }

    @Test
    @DisplayName("should encode a component argument as an object holding its client id")
    void shouldEncodeComponentArgumentAsObjectHoldingItsClientId() {
      ComponentMock arg = mock(ComponentMock.class);
      when(arg.getClientComponentId()).thenReturn("12");

      String script = component.buildCallJsFunctionScript("testFunction", true, arg);

      assertEquals("[{\"__webforjClientComponentId\":\"12\"}]", readArguments(script));
    }

    @Test
    @DisplayName("should encode a string argument as plain data")
    void shouldEncodeStringArgumentAsPlainData() {
      String script = component.buildCallJsFunctionScript("testFunction", true, "objects.get(12)");

      assertEquals("[\"objects.get(12)\"]", readArguments(script));
      assertFalse(script.contains("eval("));
    }

    @Test
    @DisplayName("should resolve a component argument by its client id")
    void shouldResolveComponentArgumentByItsClientId() {
      String script = component.buildCallJsFunctionScript("testFunction", true);

      assertTrue(script.contains("return objects.get(arg['__webforjClientComponentId']);"));
    }

    private String readArguments(String script) {
      int start = script.indexOf('`') + 1;
      int end = script.indexOf('`', start);
      byte[] arguments = Base64.getDecoder().decode(script.substring(start, end));

      return new String(arguments, StandardCharsets.UTF_8);
    }
  }

  @Nested
  @DisplayName("callJsFunctionAsync Method")
  class CallJsFunctionAsyncMethod {

    @Test
    @DisplayName("Should wait for components to be attached before invoking JS function")
    void shouldWaitForComponentsToBeAttached() {
      // Mock the component
      Element spy = spy(component);
      when(spy.isDefined()).thenReturn(true);
      PendingResult<Object> executePending = PendingResult.completedWith("call result");
      when(spy.executeJsAsync(anyString())).thenReturn(executePending);

      // Mock the container
      Element container = mock(Element.class);
      container.add(spy);

      // Mock the argument Component
      ComponentMock arg = mock(ComponentMock.class);
      PendingResult<Component> argPending = new PendingResult<>();
      when(arg.whenAttached()).thenReturn(argPending);

      // Call the function
      AtomicReference<Object> ref = new AtomicReference<>();
      PendingResult<Object> callResult = spy.callJsFunctionAsync("testFunction", arg);
      callResult.thenAccept(ref::set);

      assertFalse(callResult.isDone());
      argPending.complete(arg);
      assertTrue(callResult.isDone());
      assertEquals("call result", ref.get());
    }
  }

  @Nested
  @DisplayName("callJsFunctionVoidAsync Method")
  class CallJsFunctionVoidAsyncMethod {

    @Test
    @DisplayName("Should wait for components to be attached before invoking JS function")
    void shouldWaitForComponentsToBeAttached() {
      // Mock the component
      Element spy = spy(component);
      when(spy.isDefined()).thenReturn(true);
      doNothing().when(spy).executeJsVoidAsync(anyString());

      // Mock the container
      Element container = mock(Element.class);
      container.add(spy);

      // Mock the argument Component
      ComponentMock arg = mock(ComponentMock.class);
      PendingResult<Component> argPending = new PendingResult<>();
      when(arg.whenAttached()).thenReturn(argPending);

      // Call the function
      spy.callJsFunctionVoidAsync("testFunction", arg);

      verify(spy, times(0)).executeJsVoidAsync(anyString());

      argPending.complete(arg);
      verify(spy, times(1)).executeJsVoidAsync(anyString());
    }
  }
}
