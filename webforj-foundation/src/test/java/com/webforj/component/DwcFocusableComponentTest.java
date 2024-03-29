package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.webforj.component.event.BlurEvent;
import com.webforj.component.event.FocusEvent;
import com.webforj.component.event.MouseEnterEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcFocusableComponentTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  DwcFocusableComponentMock component;

  @Nested
  @DisplayName("Enabled API")
  class EnabledApi {

    @Test
    @DisplayName("Setting/getting enabled when control is null")
    void settingGettingEnabledWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setEnabled(true);
      assertTrue(component.isEnabled());

      verify(control, times(0)).setEnabled(true);
      verify(control, times(0)).isEnabled();
    }

    @Test
    @DisplayName("Setting/getting enabled when control is not null")
    void settingGettingReadOnlyWhenControlIsNotNull() throws BBjException {
      when(control.isEnabled()).thenReturn(true);
      component.setEnabled(true);

      assertTrue(component.isEnabled());

      verify(control, times(1)).setEnabled(true);
      verify(control, times(1)).isEnabled();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setEnabled(anyBoolean());
      assertThrows(WebforjRuntimeException.class, () -> component.setEnabled(true));

      doThrow(BBjException.class).when(control).isEnabled();
      assertThrows(WebforjRuntimeException.class, () -> component.isEnabled());
    }

    @Test
    @DisplayName("onAttach will re-apply enabled changes")
    void onAttachWillReApplyingReadOnlyChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      ReflectionUtils.nullifyControl(component);
      component.setEnabled(false);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();
      verify(control, times(1)).setEnabled(false);
    }
  }

  @Nested
  @DisplayName("Focus API")
  class TabTraversableApi {

    @Test
    @DisplayName("Setting/getting Focusable when control is null")
    void settingGettingFocusableWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setFocusable(true);

      assertTrue(component.isFocusable());
      verify(control, times(0)).setFocusable(true);
    }

    @Test
    @DisplayName("Setting/getting Focusable when control is not null")
    void settingGettingFocusableWhenControlIsNotNull() throws BBjException {
      component.setFocusable(true);
      assertTrue(component.isFocusable());
      verify(control, times(1)).setFocusable(true);
    }

    @Test
    @DisplayName("can focus component when control is not null")
    void canFocusComponent() throws BBjException {
      component.focus();
      verify(control, times(1)).focus();
    }

    @Test
    @DisplayName("When throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).focus();
      doThrow(BBjException.class).when(control).setFocusable(anyBoolean());

      assertThrows(WebforjRuntimeException.class, () -> component.focus());
      assertThrows(WebforjRuntimeException.class, () -> component.setFocusable(true));
    }

    @Test
    @DisplayName("onAttach will re-apply focus changes")
    void onAttachWillReApplyingFocusChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      ReflectionUtils.nullifyControl(component);
      component.focus();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();
      verify(control, times(1)).focus();
    }

    @Test
    @DisplayName("componentHasFocus when control is defined")
    void hasFocusWhenControlIsDefined() throws BBjException {
      doReturn("true").when(control).getProperty("hasFocus", Object.class);
      assertTrue(component.componentHasFocus());
    }

    @Test
    @DisplayName("componentHasFocus when control is null")
    void hasFocusWhenControlIsNull() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      assertFalse(component.componentHasFocus());
    }

    @Test
    @DisplayName("can handle focus events")
    void canHandleFocusEvents() {
      EventListener<FocusEvent> focusListener = event -> {
      };
      EventListener<BlurEvent> bluListener = event -> {
      };

      ListenerRegistration<FocusEvent> r1 = component.onFocus(focusListener);
      ListenerRegistration<BlurEvent> r2 = component.onBlur(bluListener);

      assertEquals(1, component.getEventListeners(FocusEvent.class).size());
      assertEquals(1, component.getEventListeners(BlurEvent.class).size());

      r1.remove();
      r2.remove();

      assertEquals(0, component.getEventListeners(FocusEvent.class).size());
      assertEquals(0, component.getEventListeners(BlurEvent.class).size());
    }
  }
}

