package org.dwcj.component.label;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.component.HorizontalAlignment.Alignment;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

/** Label tests. */
@ExtendWith(MockitoExtension.class)
public class LabelTest {

  @Mock
  BBjStaticText control;

  @Mock
  MouseEnterEventSink mouseEnterEventSink;

  @Mock
  MouseExitEventSink mouseExitEventSink;

  @Mock
  RightMouseDownEventSink rightMouseDownEventSink;

  @Spy
  EventDispatcher dispatcher;

  @InjectMocks
  Label component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Nested
  @DisplayName("Constructor")
  class Constructors {

    @Test
    @DisplayName("Constructor with text and wrap")
    void textAndWrapConstructor() {
      Label component = new Label("text", false);
      assertEquals("text", component.getText());
      assertFalse(component.isWrap());
    }
  }

  @Nested
  @DisplayName("catchUp behavior")
  class CatchUp {

    void invokeCatchUp(Label component)
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      MethodUtils.invokeMethod(component, true, "catchUp");
    }

    @Test
    @DisplayName("calling twice should not be allowed")
    void callingTwiceShouldNotBeAllowed()
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      Label componentSpy = spy(component);
      invokeCatchUp(componentSpy);
      assertThrows(InvocationTargetException.class, () -> {
        invokeCatchUp(componentSpy);
      });
    }

    @Test
    @DisplayName("catchup method")
    void catchup() throws BBjException, NoSuchMethodException, IllegalAccessException,
        InvocationTargetException {
      Label componentSpy = spy(component);

      componentSpy.setWrap(false);
      componentSpy.setHorizontalAlignment(Alignment.MIDDLE);
      componentSpy.onMouseEnter(e -> {
      });
      componentSpy.onMouseExit(e -> {
      });
      componentSpy.onRightMouseDown(e -> {
      });

      invokeCatchUp(componentSpy);

      verify(componentSpy, atLeast(2)).setWrap(false);
      verify(componentSpy, atLeast(2)).setHorizontalAlignment(Alignment.MIDDLE);
    }
  }

  @Nested
  @DisplayName("Wrap API")
  class Wrap {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setWrap(false);
      assertFalse(component.isWrap());

      verify(control, times(1)).setLineWrap(anyBoolean());
      verify(control, times(0)).getLineWrap();
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setWrap(false);
      assertFalse(component.isWrap());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setLineWrap(anyBoolean());
      assertThrows(DwcjRuntimeException.class, () -> component.setWrap(false));
    }
  }

  @Nested
  @DisplayName("Text Alignment API")
  class TextAlignment {
    @ParameterizedTest
    @EnumSource(Alignment.class)
    @DisplayName("When control is defined")
    void whenControlIsDefined(Alignment align) throws BBjException {
      component.setHorizontalAlignment(align);
      assertSame(component.getHorizontalAlignment(), align);

      verify(control, times(1)).setAlignment(anyInt());
      verify(control, times(0)).getAlignment();
    }

    @ParameterizedTest
    @EnumSource(Alignment.class)
    @DisplayName("When control is null")
    void whenControlIsNull(Alignment align) throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setHorizontalAlignment(align);
      assertSame(component.getHorizontalAlignment(), align);
    }

    @ParameterizedTest
    @EnumSource(Alignment.class)
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException(Alignment align) throws BBjException {
      doThrow(BBjException.class).when(control).setAlignment(anyInt());
      assertThrows(DwcjRuntimeException.class, () -> component.setHorizontalAlignment(align));
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
    @DisplayName("removeListener when dispatcher has already more than one MouseEnterEvent listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneMouseEnterEventListenerRegistered() {
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
    @DisplayName("removeListener when dispatcher has already more than one MouseExitEvent listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneMouseExitEventListenerRegistered() {
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
    @DisplayName("addListener when dispatcher has already RightMouseDownEvent listener registered")
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
    @DisplayName("removeListener when dispatcher has already more than one RightMouseDownEvent listener registered")
    void removeListenerWhenDispatcherHasAlreadyMoreThanOneRightMouseDownEventListenerRegistered() {
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
}
