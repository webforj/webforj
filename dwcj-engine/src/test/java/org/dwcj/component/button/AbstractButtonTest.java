package org.dwcj.component.button;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class AbstractButtonTest {

  @Mock
  BBjButton control;

  @InjectMocks
  AbstractButtonMock component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  void invokeCatchUp()
      throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
    FieldUtils.writeField(component, "control", control, true);
    MethodUtils.invokeMethod(component, true, "catchUp");
  }

  @Nested
  @DisplayName("Focus API")
  class FocusApi {
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
  @DisplayName("DisableOnclick API")
  class DisableOnclickApi {
    @Test
    @DisplayName("disableOnClick when control is defined")
    void disableOnClickWhenControlIsDefined() throws BBjException {
      doReturn(true).when(control).getDisableOnClick();

      component.setDisableOnClick(true);
      assertTrue(component.isDisableOnClick());

      verify(control, times(1)).setDisableOnClick(true);
      verify(control, times(1)).getDisableOnClick();
    }

    @Test
    @DisplayName("disableOnClick when control is null")
    void disableOnClickWhenControlIsNull() throws BBjException, IllegalAccessException,
        NoSuchMethodException, InvocationTargetException {
      nullifyControl();

      component.setDisableOnClick(true);
      assertTrue(component.isDisableOnClick());

      verify(control, times(0)).setDisableOnClick(true);
      verify(control, times(0)).getDisableOnClick();

      invokeCatchUp();

      verify(control, times(1)).setDisableOnClick(true);
    }

    @Test
    @DisplayName("""
            set/is disableOnClick re-throw DwcjRuntimeException when a BBjException is thrown
        """)
    void disableOnClickReThrowDwcjRuntimeExceptionWhenABbjExceptionIsThrown() throws BBjException,
        IllegalAccessException, NoSuchMethodException, InvocationTargetException {
      doThrow(BBjException.class).when(control).setDisableOnClick(true);
      doThrow(BBjException.class).when(control).getDisableOnClick();

      assertThrows(DwcjRuntimeException.class, () -> component.setDisableOnClick(true));
      assertThrows(DwcjRuntimeException.class, () -> component.isDisableOnClick());
    }
  }


  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    @DisplayName("adding/removing supported events")
    void addingRemovingSupportedEvents() {
      EventListener<ButtonClickEvent> clickListener = event -> {
      };
      EventListener<FocusEvent> focusListener = event -> {
      };
      EventListener<BlurEvent> blurListener = event -> {
      };
      EventListener<MouseEnterEvent> mouseEnterListener = event -> {
      };
      EventListener<MouseExitEvent> mouseExitListener = event -> {
      };
      EventListener<RightMouseDownEvent> rightMouseDownListener = event -> {
      };

      component.onClick(clickListener);
      component.onFocus(focusListener);
      component.onBlur(blurListener);
      component.onMouseEnter(mouseEnterListener);
      component.onMouseExit(mouseExitListener);
      component.onRightMouseDown(rightMouseDownListener);

      EventDispatcher dispatcher = component.getEventDispatcher();

      assertEquals(1, dispatcher.getListenersCount(ButtonClickEvent.class));
      assertEquals(1, dispatcher.getListenersCount(FocusEvent.class));
      assertEquals(1, dispatcher.getListenersCount(BlurEvent.class));
      assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));
      assertEquals(1, dispatcher.getListenersCount(MouseExitEvent.class));
      assertEquals(1, dispatcher.getListenersCount(RightMouseDownEvent.class));

      component.removeClickListener(clickListener);
      component.removeFocusListener(focusListener);
      component.removeBlurListener(blurListener);
      component.removeMouseEnterListener(mouseEnterListener);
      component.removeMouseExitListener(mouseExitListener);
      component.removeRightMouseDownListener(rightMouseDownListener);

      assertEquals(0, dispatcher.getListenersCount(ButtonClickEvent.class));
      assertEquals(0, dispatcher.getListenersCount(FocusEvent.class));
      assertEquals(0, dispatcher.getListenersCount(BlurEvent.class));
      assertEquals(0, dispatcher.getListenersCount(MouseEnterEvent.class));
      assertEquals(0, dispatcher.getListenersCount(MouseExitEvent.class));
      assertEquals(0, dispatcher.getListenersCount(RightMouseDownEvent.class));
    }
  }
}
