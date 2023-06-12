package org.dwcj.component.htmlEdit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjHtmlEdit;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.htmledit.HtmlEdit;
import org.dwcj.component.htmledit.event.PageLoadedEvent;
import org.dwcj.component.htmledit.event.StateChangeEvent;
import org.dwcj.component.htmledit.sink.PageLoadedEventSink;
import org.dwcj.component.htmledit.sink.StateChangeEventSink;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;


/** HtmlEdit tests. */
@ExtendWith(MockitoExtension.class)
public class HtmlEditTest {
  @Mock
  BBjHtmlEdit control;

  @Mock
  MouseEnterEventSink mouseEnterEventSink;

  @Mock
  MouseExitEventSink mouseExitEventSink;

  @Mock
  RightMouseDownEventSink rightMouseDownEventSink;

  @Mock
  FocusEventSink focusEventSink;

  @Mock
  BlurEventSink blurEventSink;

  @Mock
  PageLoadedEventSink pageLoadedEventSink;

  @Mock
  StateChangeEventSink stateChangeEventSink;

  @Spy
  EventDispatcher dispatcher;

  @InjectMocks
  HtmlEdit component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Nested
  @DisplayName("Toolbar API")
  class Toolbar {

    @Test
    @DisplayName("Alltoolbar When control is defined")
    void allWhenControlisdefined() throws BBjException {
      component.getAllToolbarStyles();
      verify(control, times(1)).getAllToolbarStyles();
    }

    @Test
    @DisplayName("Alltoolbar when control is null")
    void allWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      assertEquals(List.of(), component.getAllToolbarStyles());
    }

    @Test
    @DisplayName("Basictoolbar When control is defined")
    void basicWhenControlisdefined() throws BBjException {
      component.getBasicToolbarStyles();
      verify(control, times(1)).getBasicToolbarStyles();
    }

    @Test
    @DisplayName("Basictoolbar when control is null")
    void basicWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      assertEquals(List.of(), component.getBasicToolbarStyles());
    }

    @Test
    @DisplayName("IsBasicToolbar when control is defined")
    void isBasicWhenControlIsDefined() throws BBjException, IllegalAccessException {
      component.setBasicToolbar(true);

      verify(control, times(1)).setBasicToolbar(anyBoolean());
      verify(control, times(0)).getBasicToolbar();
    }

    @Test
    @DisplayName("IsBasicToolbar when control is null")
    void isBasicWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setBasicToolbar(true);
      assertTrue(component.isBasicToolbar());
    }



  }

  @Nested
  @DisplayName("Client API")
  class Client {

    @Test
    @DisplayName("ClientType when control is defined")
    void typeWhenControlisdefined() throws BBjException {
      component.getClientType();
      verify(control, times(1)).getClientType();
    }

    @Test
    @DisplayName("ClientType when control is null")
    void typeWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      assertEquals("", component.getClientType());
    }

    @Test
    @DisplayName("ClientVersion when control is defined")
    void versionWhenControlisdefined() throws BBjException {
      component.getClientVersion();
      verify(control, times(1)).getClientVersion();
    }

    @Test
    @DisplayName("ClientVersion when control is null")
    void versionWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      assertEquals("", component.getClientVersion());
    }

  }

  @Nested
  @DisplayName("Locale API")
  class Locale {

    @Test
    @DisplayName("Locale when control is defined")
    void typeWhenControlisdefined() throws BBjException {
      component.getLocale();
      verify(control, times(1)).getLocale();
    }

    @Test
    @DisplayName("Locale when control is null")
    void typeWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      assertEquals("", component.getLocale());
    }

  }

  @Nested
  @DisplayName("Plaintext API")
  class PlainText {

    @Test
    @DisplayName("Get when control is defined")
    void getWhenControlisdefined() throws BBjException {
      component.getPlainText();
      verify(control, times(1)).getPlainText();
    }

    @Test
    @DisplayName("Get when control is null")
    void getWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      assertEquals("", component.getPlainText());
    }

    @Test
    @DisplayName("Set when control is defined")
    void setWhenControlisdefined() throws BBjException {
      component.setPlainText("test");
      verify(control, times(1)).setPlainText(anyString());
    }

    @Test
    @DisplayName("Set when control is null")
    void setWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setPlainText("test");

      assertEquals("test", component.getPlainText());
    }

  }



  @Nested
  @DisplayName("Mouse Enter Events")
  class MouseEnter {

    @Test
    @DisplayName("addListener when control is defined")
    void addListenerWhenControlIsDefined() {
      EventListener<MouseEnterEvent> listener = e -> {
        // do nothing
      };
      component.onMouseEnter(listener);

      verify(mouseEnterEventSink, times(1)).setCallback();
      verify(dispatcher, times(1)).addEventListener(MouseEnterEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when control is null")
    void addListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<MouseEnterEvent> listener = e -> {
        // do nothing
      };
      component.onMouseEnter(listener);
      verify(mouseEnterEventSink, times(0)).setCallback();
      verify(dispatcher, times(1)).addEventListener(MouseEnterEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when dispatcher has already MouseEnterEvent listener registered")
    void addListenerWhenDispatcherHasAlreadyMouseEnterEventListenerRegistered() {
      EventListener<MouseEnterEvent> listener = e -> {
        // do nothing
      };

      dispatcher.addEventListener(MouseEnterEvent.class, listener);
      assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));

      component.onMouseEnter(e -> {
        // do nothing
      });
      verify(mouseEnterEventSink, times(0)).setCallback();
    }

    @Test
    @DisplayName("removeListener when control is defined")
    void removeListenerWhenControlIsDefined() {
      EventListener<MouseEnterEvent> listener = e -> {
        // do nothing
      };
      component.onMouseEnter(listener);
      component.removeMouseEnterListener(listener);

      verify(mouseEnterEventSink, times(1)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(MouseEnterEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when control is null")
    void removeListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<MouseEnterEvent> listener = e -> {
        // do nothing
      };
      component.onMouseEnter(listener);
      component.removeMouseEnterListener(listener);

      verify(mouseEnterEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(MouseEnterEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when dispatcher has already more than one listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneListenerRegistered() {
      EventListener<MouseEnterEvent> listener = e -> {
        // do nothing
      };
      EventListener<MouseEnterEvent> listener2 = e -> {
        // do nothing
      };

      component.onMouseEnter(listener);
      component.onMouseEnter(listener2);
      assertEquals(2, dispatcher.getListenersCount(MouseEnterEvent.class));

      component.removeMouseEnterListener(listener);
      verify(mouseEnterEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(MouseEnterEvent.class, listener);
    }
  }

  @Nested
  @DisplayName("Mouse Exit Events")
  class MouseExit {

    @Test
    @DisplayName("addListener when control is defined")
    void addListenerWhenControlIsDefined() {
      EventListener<MouseExitEvent> listener = e -> {
        // do nothing
      };
      component.onMouseExit(listener);

      verify(mouseExitEventSink, times(1)).setCallback();
      verify(dispatcher, times(1)).addEventListener(MouseExitEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when control is null")
    void addListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<MouseExitEvent> listener = e -> {
        // do nothing
      };
      component.onMouseExit(listener);
      verify(mouseExitEventSink, times(0)).setCallback();
      verify(dispatcher, times(1)).addEventListener(MouseExitEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when dispatcher has already MouseExitEvent listener registered")
    void addListenerWhenDispatcherHasAlreadyMouseExitEventListenerRegistered() {
      EventListener<MouseExitEvent> listener = e -> {
        // do nothing
      };

      dispatcher.addEventListener(MouseExitEvent.class, listener);
      assertEquals(1, dispatcher.getListenersCount(MouseExitEvent.class));

      component.onMouseExit(e -> {
        // do nothing
      });
      verify(mouseExitEventSink, times(0)).setCallback();
    }

    @Test
    @DisplayName("removeListener when control is defined")
    void removeListenerWhenControlIsDefined() {
      EventListener<MouseExitEvent> listener = e -> {
        // do nothing
      };
      component.onMouseExit(listener);
      component.removeMouseExitListener(listener);

      verify(mouseExitEventSink, times(1)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(MouseExitEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when control is null")
    void removeListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<MouseExitEvent> listener = e -> {
        // do nothing
      };
      component.onMouseExit(listener);
      component.removeMouseExitListener(listener);

      verify(mouseExitEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(MouseExitEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when dispatcher has already more than one listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneListenerRegistered() {
      EventListener<MouseExitEvent> listener = e -> {
        // do nothing
      };
      EventListener<MouseExitEvent> listener2 = e -> {
        // do nothing
      };

      component.onMouseExit(listener);
      component.onMouseExit(listener2);
      assertEquals(2, dispatcher.getListenersCount(MouseExitEvent.class));

      component.removeMouseExitListener(listener);
      verify(mouseExitEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(MouseExitEvent.class, listener);
    }
  }

  @Nested
  @DisplayName("Right Mouse Down Events")
  class RightMouseDown {

    @Test
    @DisplayName("addListener when control is defined")
    void addListenerWhenControlIsDefined() {
      EventListener<RightMouseDownEvent> listener = e -> {
        // do nothing
      };
      component.onRightMouseDown(listener);

      verify(rightMouseDownEventSink, times(1)).setCallback();
      verify(dispatcher, times(1)).addEventListener(RightMouseDownEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when control is null")
    void addListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<RightMouseDownEvent> listener = e -> {
        // do nothing
      };
      component.onRightMouseDown(listener);
      verify(rightMouseDownEventSink, times(0)).setCallback();
      verify(dispatcher, times(1)).addEventListener(RightMouseDownEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when dispatcher has already listener registered")
    void addListenerWhenDispatcherHasAlreadyRightMouseDownEventListenerRegistered() {
      EventListener<RightMouseDownEvent> listener = e -> {
        // do nothing
      };

      dispatcher.addEventListener(RightMouseDownEvent.class, listener);
      assertEquals(1, dispatcher.getListenersCount(RightMouseDownEvent.class));

      component.onRightMouseDown(e -> {
        // do nothing
      });
      verify(rightMouseDownEventSink, times(0)).setCallback();
    }

    @Test
    @DisplayName("removeListener when control is defined")
    void removeListenerWhenControlIsDefined() {
      EventListener<RightMouseDownEvent> listener = e -> {
        // do nothing
      };
      component.onRightMouseDown(listener);
      component.removeRightMouseDownListener(listener);

      verify(rightMouseDownEventSink, times(1)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(RightMouseDownEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when control is null")
    void removeListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<RightMouseDownEvent> listener = e -> {
        // do nothing
      };
      component.onRightMouseDown(listener);
      component.removeRightMouseDownListener(listener);

      verify(rightMouseDownEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(RightMouseDownEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when dispatcher has already more than one listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneListenerRegistered() {
      EventListener<RightMouseDownEvent> listener = e -> {
        // do nothing
      };
      EventListener<RightMouseDownEvent> listener2 = e -> {
        // do nothing
      };

      component.onRightMouseDown(listener);
      component.onRightMouseDown(listener2);
      assertEquals(2, dispatcher.getListenersCount(RightMouseDownEvent.class));

      component.removeRightMouseDownListener(listener);
      verify(rightMouseDownEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(RightMouseDownEvent.class, listener);
    }
  }

  @Nested
  @DisplayName("Focus Events")
  class Focus {

    @Test
    @DisplayName("addListener when control is defined")
    void addListenerWhenControlIsDefined() {
      EventListener<FocusEvent> listener = e -> {
        // do nothing
      };
      component.onFocus(listener);

      verify(focusEventSink, times(1)).setCallback();
      verify(dispatcher, times(1)).addEventListener(FocusEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when control is null")
    void addListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<FocusEvent> listener = e -> {
        // do nothing
      };
      component.onFocus(listener);
      verify(focusEventSink, times(0)).setCallback();
      verify(dispatcher, times(1)).addEventListener(FocusEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when dispatcher has already listener registered")
    void addListenerWhenDispatcherHasAlreadyFocusEventListenerRegistered() {
      EventListener<FocusEvent> listener = e -> {
        // do nothing
      };

      dispatcher.addEventListener(FocusEvent.class, listener);
      assertEquals(1, dispatcher.getListenersCount(FocusEvent.class));

      component.onRightMouseDown(e -> {
        // do nothing
      });
      verify(focusEventSink, times(0)).setCallback();
    }

    @Test
    @DisplayName("removeListener when control is defined")
    void removeListenerWhenControlIsDefined() {
      EventListener<FocusEvent> listener = e -> {
        // do nothing
      };
      component.onFocus(listener);
      component.removeFocusListener(listener);

      verify(focusEventSink, times(1)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(FocusEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when control is null")
    void removeListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<FocusEvent> listener = e -> {
        // do nothing
      };
      component.onFocus(listener);
      component.removeFocusListener(listener);

      verify(focusEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(FocusEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when dispatcher has already more than one listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneListenerRegistered() {
      EventListener<FocusEvent> listener = e -> {
        // do nothing
      };
      EventListener<FocusEvent> listener2 = e -> {
        // do nothing
      };

      component.onFocus(listener);
      component.onFocus(listener2);
      assertEquals(2, dispatcher.getListenersCount(FocusEvent.class));

      component.removeFocusListener(listener);
      verify(focusEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(FocusEvent.class, listener);
    }
  }

  @Nested
  @DisplayName("Blur Events")
  class Blur {

    @Test
    @DisplayName("addListener when control is defined")
    void addListenerWhenControlIsDefined() {
      EventListener<BlurEvent> listener = e -> {
        // do nothing
      };
      component.onBlur(listener);

      verify(blurEventSink, times(1)).setCallback();
      verify(dispatcher, times(1)).addEventListener(BlurEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when control is null")
    void addListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<BlurEvent> listener = e -> {
        // do nothing
      };
      component.onBlur(listener);
      verify(blurEventSink, times(0)).setCallback();
      verify(dispatcher, times(1)).addEventListener(BlurEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when dispatcher has already listener registered")
    void addListenerWhenDispatcherHasAlreadyBlurEventListenerRegistered() {
      EventListener<BlurEvent> listener = e -> {
        // do nothing
      };

      dispatcher.addEventListener(BlurEvent.class, listener);
      assertEquals(1, dispatcher.getListenersCount(BlurEvent.class));

      component.onRightMouseDown(e -> {
        // do nothing
      });
      verify(blurEventSink, times(0)).setCallback();
    }

    @Test
    @DisplayName("removeListener when control is defined")
    void removeListenerWhenControlIsDefined() {
      EventListener<BlurEvent> listener = e -> {
        // do nothing
      };
      component.onBlur(listener);
      component.removeBlurListener(listener);

      verify(blurEventSink, times(1)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(BlurEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when control is null")
    void removeListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<BlurEvent> listener = e -> {
        // do nothing
      };
      component.onBlur(listener);
      component.removeBlurListener(listener);

      verify(blurEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(BlurEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when dispatcher has already more than one listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneListenerRegistered() {
      EventListener<BlurEvent> listener = e -> {
        // do nothing
      };
      EventListener<BlurEvent> listener2 = e -> {
        // do nothing
      };

      component.onBlur(listener);
      component.onBlur(listener2);
      assertEquals(2, dispatcher.getListenersCount(BlurEvent.class));

      component.removeBlurListener(listener);
      verify(blurEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(BlurEvent.class, listener);
    }
  }

  @Nested
  @DisplayName("PageLoaded Events")
  class PageLoaded {

    @Test
    @DisplayName("addListener when control is defined")
    void addListenerWhenControlIsDefined() {
      EventListener<PageLoadedEvent> listener = e -> {
        // do nothing
      };
      component.onPageLoaded(listener);

      verify(pageLoadedEventSink, times(1)).setCallback();
      verify(dispatcher, times(1)).addEventListener(PageLoadedEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when control is null")
    void addListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<PageLoadedEvent> listener = e -> {
        // do nothing
      };
      component.onPageLoaded(listener);
      verify(pageLoadedEventSink, times(0)).setCallback();
      verify(dispatcher, times(1)).addEventListener(PageLoadedEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when dispatcher has already listener registered")
    void addListenerWhenDispatcherHasAlreadyPageLoadedEventListenerRegistered() {
      EventListener<PageLoadedEvent> listener = e -> {
        // do nothing
      };

      dispatcher.addEventListener(PageLoadedEvent.class, listener);
      assertEquals(1, dispatcher.getListenersCount(PageLoadedEvent.class));

      component.onRightMouseDown(e -> {
        // do nothing
      });
      verify(pageLoadedEventSink, times(0)).setCallback();
    }

    @Test
    @DisplayName("removeListener when control is defined")
    void removeListenerWhenControlIsDefined() {
      EventListener<PageLoadedEvent> listener = e -> {
        // do nothing
      };
      component.onPageLoaded(listener);
      component.removePageLoadedListener(listener);

      verify(pageLoadedEventSink, times(1)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(PageLoadedEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when control is null")
    void removeListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<PageLoadedEvent> listener = e -> {
        // do nothing
      };
      component.onPageLoaded(listener);
      component.removePageLoadedListener(listener);

      verify(pageLoadedEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(PageLoadedEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when dispatcher has already more than one listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneListenerRegistered() {
      EventListener<PageLoadedEvent> listener = e -> {
        // do nothing
      };
      EventListener<PageLoadedEvent> listener2 = e -> {
        // do nothing
      };

      component.onPageLoaded(listener);
      component.onPageLoaded(listener2);
      assertEquals(2, dispatcher.getListenersCount(PageLoadedEvent.class));

      component.removePageLoadedListener(listener);
      verify(pageLoadedEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(PageLoadedEvent.class, listener);
    }
  }

  @Nested
  @DisplayName("StateChanged Events")
  class StateChanged {

    @Test
    @DisplayName("addListener when control is defined")
    void addListenerWhenControlIsDefined() {
      EventListener<StateChangeEvent> listener = e -> {
        // do nothing
      };
      component.onStateChange(listener);

      verify(stateChangeEventSink, times(1)).setCallback();
      verify(dispatcher, times(1)).addEventListener(StateChangeEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when control is null")
    void addListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<StateChangeEvent> listener = e -> {
        // do nothing
      };
      component.onStateChange(listener);
      verify(stateChangeEventSink, times(0)).setCallback();
      verify(dispatcher, times(1)).addEventListener(StateChangeEvent.class, listener);
    }

    @Test
    @DisplayName("addListener when dispatcher has already listener registered")
    void addListenerWhenDispatcherHasAlreadyStateChangeEventListenerRegistered() {
      EventListener<StateChangeEvent> listener = e -> {
        // do nothing
      };

      dispatcher.addEventListener(StateChangeEvent.class, listener);
      assertEquals(1, dispatcher.getListenersCount(StateChangeEvent.class));

      component.onRightMouseDown(e -> {
        // do nothing
      });
      verify(stateChangeEventSink, times(0)).setCallback();
    }

    @Test
    @DisplayName("removeListener when control is defined")
    void removeListenerWhenControlIsDefined() {
      EventListener<StateChangeEvent> listener = e -> {
        // do nothing
      };
      component.onStateChange(listener);
      component.removeStateChangeListener(listener);

      verify(stateChangeEventSink, times(1)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(StateChangeEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when control is null")
    void removeListenerWhenControlIsNull() throws IllegalAccessException {
      nullifyControl();
      EventListener<StateChangeEvent> listener = e -> {
        // do nothing
      };
      component.onStateChange(listener);
      component.removeStateChangeListener(listener);

      verify(stateChangeEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(StateChangeEvent.class, listener);
    }

    @Test
    @DisplayName("removeListener when dispatcher has already more than one listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneListenerRegistered() {
      EventListener<StateChangeEvent> listener = e -> {
        // do nothing
      };
      EventListener<StateChangeEvent> listener2 = e -> {
        // do nothing
      };

      component.onStateChange(listener);
      component.onStateChange(listener2);
      assertEquals(2, dispatcher.getListenersCount(StateChangeEvent.class));

      component.removeStateChangeListener(listener);
      verify(stateChangeEventSink, times(0)).removeCallback();
      verify(dispatcher, times(1)).removeEventListener(StateChangeEvent.class, listener);
    }
  }


  @Nested
  @DisplayName("catchUp behavior")
  class CatchUp {

    void invokeCatchUp(HtmlEdit component)
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      MethodUtils.invokeMethod(component, true, "catchUp");
    }

    @Test
    @DisplayName("calling twice should not be allowed")
    void callingTwiceShouldNotBeAllowed()
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      HtmlEdit componentSpy = spy(component);
      invokeCatchUp(componentSpy);
      assertThrows(InvocationTargetException.class, () -> {
        invokeCatchUp(componentSpy);
      });
    }

    @Test
    @DisplayName("catchup method")
    void catchup() throws BBjException, NoSuchMethodException, IllegalAccessException,
        InvocationTargetException {
      HtmlEdit componentSpy = spy(component);

      componentSpy.setBasicToolbar(true);
      componentSpy.setLocale("test");
      componentSpy.setPlainText("test");
      componentSpy.setState("test", false);

      componentSpy.onMouseEnter(e -> {
      });
      componentSpy.onMouseExit(e -> {
      });
      componentSpy.onRightMouseDown(e -> {
      });

      invokeCatchUp(componentSpy);

      verify(componentSpy, atLeast(2)).setBasicToolbar(anyBoolean());
      verify(componentSpy, atLeast(2)).setLocale("test");
      verify(componentSpy, atLeast(2)).setPlainText("test");
      verify(componentSpy, atLeast(2)).setState("test", false);


    }
  }


}
