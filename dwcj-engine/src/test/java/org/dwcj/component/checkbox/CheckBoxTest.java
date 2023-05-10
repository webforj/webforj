package org.dwcj.component.checkbox;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.CheckedEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.event.sink.UncheckedEventSink;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;


/** Checkbox tests. */
@ExtendWith(MockitoExtension.class)
public class CheckBoxTest {

  @Mock
  BBjCheckBox control;

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
  CheckedEventSink checkedEventSink;

  @Mock
  UncheckedEventSink unCheckedEventSink;

  @Spy
  EventDispatcher dispatcher;

  @InjectMocks
  CheckBox component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Nested
  @DisplayName("catchUp behavior")
  class CatchUp {

    void invokeCatchUp(CheckBox component)
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      MethodUtils.invokeMethod(component, true, "catchUp");
    }

    @Test
    @DisplayName("calling twice should not be allowed")
    void callingTwiceShouldNotBeAllowed()
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
      CheckBox componentSpy = spy(component);
      invokeCatchUp(componentSpy);
      assertThrows(InvocationTargetException.class, () -> {
        invokeCatchUp(componentSpy);
      });
    }

    @Test
    @DisplayName("catchup method")
    void catchup() throws BBjException, NoSuchMethodException, IllegalAccessException,
        InvocationTargetException {
      CheckBox componentSpy = spy(component);

      componentSpy.setChecked(true);
      componentSpy.setReadOnly(true);
      componentSpy.setTabTraversable(false);
      // componentSpy.setTextPosition(Position.LEFT);
      componentSpy.setIndeterminate(true);
      componentSpy.setName("test");
      componentSpy.setRequired(true);

      invokeCatchUp(componentSpy);

      verify(componentSpy, atLeast(2)).setChecked(true);
      verify(componentSpy, atLeast(2)).setReadOnly(true);
      verify(componentSpy, atLeast(2)).setTabTraversable(false);
      verify(componentSpy, atLeast(2)).setIndeterminate(true);
      verify(componentSpy, atLeast(2)).setName("test");
      verify(componentSpy, atLeast(2)).setRequired(true);

    }
  }

  @Nested
  @DisplayName("Checked API")
  class Checked {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setChecked(true);
      component.isChecked();

      verify(control, times(1)).setSelected(anyBoolean());
      verify(control, times(1)).isSelected();

    }

    @Test
    @DisplayName("When Control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setChecked(true);
      assertTrue(component.isChecked());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setSelected(anyBoolean());
      assertThrows(DwcjRuntimeException.class, () -> component.setChecked(true));
    }
  }

  @Nested
  @DisplayName("Indeterminate API")
  class Indeterminate {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setIndeterminate(true);
      assertTrue(component.getIndeterminate());
      verify(control, times(1)).setAttribute("indeterminate", "true");
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setIndeterminate(true);
      assertTrue(component.getIndeterminate());
    }
  }

  @Nested
  @DisplayName("Label API")
  class Label {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setLabel("test");
      component.getLabel();

      verify(control, times(1)).putClientProperty("label", "test");
      verify(control, times(1)).getClientProperty("label");
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setLabel("test");
      assertEquals("test", component.getLabel());
    }
  }

  @Nested
  @DisplayName("Name API")
  class Name {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setName("test");
      component.getName();

      verify(control, times(1)).setAttribute("name", "test");
      verify(control, times(0)).getAttribute("name");
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setName("test");
      assertEquals("test", component.getName());
    }
  }

  @Nested
  @DisplayName("Required API")
  class Required {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setRequired(true);
      component.getRequired();

      verify(control, times(1)).setAttribute("required", "true");
      verify(control, times(0)).getAttribute("required");
    }

    @Test
    @DisplayName("When control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setRequired(true);
      assertEquals(true, component.getRequired());
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
}
