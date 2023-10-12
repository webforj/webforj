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
import org.dwcj.component.ReflectionUtils;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AbstractDwcButtonTest {

  @Mock
  BBjButton control;

  @InjectMocks
  AbstractDwcButtonMock component;

  @Test
  void testSetGetName() throws BBjException, IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    assertEquals("", component.getName());

    component.setName("name");
    assertEquals("name", component.getName());
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
      ReflectionUtils.nullifyControl(component);
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
    void disableOnClickWhenControlIsNull() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setDisableOnClick(true);
      assertTrue(component.isDisableOnClick());

      verify(control, times(0)).setDisableOnClick(true);
      verify(control, times(0)).getDisableOnClick();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setDisableOnClick(true);
    }

    @Test
    @DisplayName("""
            set/is disableOnClick re-throw DwcjRuntimeException when a BBjException is thrown
        """)
    void disableOnClickReThrowDwcjRuntimeExceptionWhenABbjExceptionIsThrown() throws BBjException {
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
      ComponentEventListener<ButtonClickEvent> clickListener = event -> {
      };

      component.onClick(clickListener);
      assertEquals(1, component.getEventListeners(ButtonClickEvent.class).size());

      component.removeClickListener(clickListener);
      assertEquals(0, component.getEventListeners(ButtonClickEvent.class).size());
    }
  }
}
