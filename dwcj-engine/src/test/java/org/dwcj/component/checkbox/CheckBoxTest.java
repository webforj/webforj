package org.dwcj.component.checkbox;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.component.TextPosition.Position;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.CheckedEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.ToggleEvent;
import org.dwcj.component.event.UncheckedEvent;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class CheckBoxTest {

  @Mock
  BBjCheckBox control;

  @InjectMocks
  CheckBox component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Nested
  @DisplayName("Checked API")
  class CheckedAPI {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      doReturn(true).when(control).isSelected();

      component.setChecked(true);
      assertTrue(component.isChecked());

      verify(control, times(1)).setSelected(true);
      verify(control, times(1)).isSelected();
    }

    @Test
    @DisplayName("When Control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setChecked(true);
      assertTrue(component.isChecked());

      verify(control, times(0)).setSelected(true);
      verify(control, times(0)).isSelected();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setSelected(anyBoolean());
      assertThrows(DwcjRuntimeException.class, () -> component.setChecked(true));

      doThrow(BBjException.class).when(control).isSelected();
      assertThrows(DwcjRuntimeException.class, () -> component.isChecked());
    }
  }

  @Nested
  @DisplayName("Indeterminate API")
  class IndeterminateAPI {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setIndeterminate(true);
      assertTrue(component.isIndeterminate());

      verify(control, times(1)).putClientProperty("indeterminate", true);
      verify(control, times(0)).getClientProperty("indeterminate");
    }

    @Test
    @DisplayName("When Control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setIndeterminate(true);
      assertTrue(component.isIndeterminate());
    }
  }

  @Nested
  @DisplayName("TextPosition API")
  class TextPositionAPI {

    @ParameterizedTest
    @EnumSource(Position.class)
    @DisplayName("When control is defined")
    void whenControlIsDefined(Position position) throws BBjException {
      component.setTextPosition(position);
      assertSame(component.getTextPosition(), position);

      verify(control, times(1)).setHorizontalTextPosition(anyInt());
      verify(control, times(0)).getHorizontalTextPosition();
    }

    @ParameterizedTest
    @EnumSource(Position.class)
    @DisplayName("When control is null")
    void whenControlIsNull(Position position) throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setTextPosition(position);
      assertSame(component.getTextPosition(), position);

      verify(control, times(0)).setHorizontalTextPosition(anyInt());
      verify(control, times(0)).getHorizontalTextPosition();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setHorizontalTextPosition(anyInt());
      assertThrows(DwcjRuntimeException.class, () -> component.setTextPosition(Position.LEFT));
    }
  }

  @Nested
  @DisplayName("Expanse API")
  class ExpanseAPI {

    @ParameterizedTest
    @EnumSource(CheckBox.Expanse.class)
    @DisplayName("Setting/getting expanse")
    void settingGettingExpanse(CheckBox.Expanse expanse) throws BBjException {
      component.setExpanse(expanse);
      assertSame(component.getExpanse(), expanse);

      verify(control, times(1)).putClientProperty("expanse", expanse.getValue());
      verify(control, times(0)).getClientProperty("expanse");
    }
  }

  @Nested
  @DisplayName("Focus API")
  class FocusAPI {
    @Test
    @DisplayName("hasFocus when control is defined")
    void hasFocusWhenControlIsDefined() throws BBjException {
      doReturn("true").when(control).getClientProperty("hasFocus");
      assertTrue(component.hasFocus());
    }

    @Test
    @DisplayName("hasFocus when control is null")
    void hasFocusWhenControlIsNull() throws BBjException, IllegalAccessException {
      nullifyControl();
      assertFalse(component.hasFocus());
    }
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
      componentSpy.setTextPosition(Position.LEFT);

      invokeCatchUp(componentSpy);

      verify(componentSpy, atLeast(2)).setChecked(true);
      verify(componentSpy, atLeast(2)).setTextPosition(Position.LEFT);
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsAPI {

    @Test
    @DisplayName("adding/removing supported events")
    void addingRemovingSupportedEvents() {
      EventDispatcher dispatcher = component.getEventDispatcher();

      EventListener<CheckedEvent> checkedListener = event -> {
      };
      EventListener<UncheckedEvent> uncheckedListener = event -> {
      };
      EventListener<ToggleEvent> toggleListener = event -> {
      };
      EventListener<MouseEnterEvent> mouseEnterListener = event -> {
      };
      EventListener<FocusEvent> focusListener = event -> {
      };
      EventListener<BlurEvent> blurListener = event -> {
      };
      EventListener<MouseExitEvent> mouseExitListener = event -> {
      };
      EventListener<RightMouseDownEvent> rightMouseDownListener = event -> {
      };

      component.onChecked(checkedListener);
      component.onUnchecked(uncheckedListener);
      component.onToggle(toggleListener);
      component.onFocus(focusListener);
      component.onBlur(blurListener);
      component.onMouseEnter(mouseEnterListener);
      component.onMouseExit(mouseExitListener);
      component.onRightMouseDown(rightMouseDownListener);

      assertEquals(1, dispatcher.getListenersCount(CheckedEvent.class));
      assertEquals(1, dispatcher.getListenersCount(UncheckedEvent.class));
      assertEquals(1, dispatcher.getListenersCount(ToggleEvent.class));
      assertEquals(1, dispatcher.getListenersCount(FocusEvent.class));
      assertEquals(1, dispatcher.getListenersCount(BlurEvent.class));
      assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));
      assertEquals(1, dispatcher.getListenersCount(MouseExitEvent.class));
      assertEquals(1, dispatcher.getListenersCount(RightMouseDownEvent.class));

      component.removeCheckedListener(checkedListener);
      component.removeUncheckedListener(uncheckedListener);
      component.removeToggleListener(toggleListener);
      component.removeFocusListener(focusListener);
      component.removeBlurListener(blurListener);
      component.removeMouseEnterListener(mouseEnterListener);
      component.removeMouseExitListener(mouseExitListener);
      component.removeRightMouseDownListener(rightMouseDownListener);

      assertEquals(0, dispatcher.getListenersCount(CheckedEvent.class));
      assertEquals(0, dispatcher.getListenersCount(UncheckedEvent.class));
      assertEquals(0, dispatcher.getListenersCount(ToggleEvent.class));
      assertEquals(0, dispatcher.getListenersCount(FocusEvent.class));
      assertEquals(0, dispatcher.getListenersCount(BlurEvent.class));
      assertEquals(0, dispatcher.getListenersCount(MouseEnterEvent.class));
      assertEquals(0, dispatcher.getListenersCount(MouseExitEvent.class));
      assertEquals(0, dispatcher.getListenersCount(RightMouseDownEvent.class));
    }
  }
}
