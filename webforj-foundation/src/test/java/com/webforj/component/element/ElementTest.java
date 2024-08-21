package com.webforj.component.element;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.startup.type.BBjException;
import com.webforj.PendingResult;
import com.webforj.component.Component;
import com.webforj.component.ComponentMock;
import com.webforj.component.DwcComponentMock;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.element.event.ElementDefinedEvent;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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
  @DisplayName("Events API")
  class EventsApi {

    @Test
    @DisplayName("Adding events with different params will return different ListenerRegistration")
    void addingEventWithDifferentParams() {
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
