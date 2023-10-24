package org.dwcj.component.optioninput;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjToggleButton;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import org.dwcj.component.ReflectionUtils;
import org.dwcj.component.event.CheckEvent;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.event.ToggleEvent;
import org.dwcj.component.event.UncheckEvent;
import org.dwcj.concern.HasTextPosition.Position;
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
class DwcOptionInputTest {

  @Mock
  BBjToggleButton control;

  @InjectMocks
  DwcOptionInputMock component;

  @Nested
  @DisplayName("Checked API")
  class CheckedApi {
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
      ReflectionUtils.nullifyControl(component);
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
  @DisplayName("TextPosition API")
  class TextPositionApi {

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
      ReflectionUtils.nullifyControl(component);
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
  @DisplayName("onAttach behavior")
  class OnAttach {

    @Test
    @DisplayName("onAttach re-apply changes to control")
    void catchup() throws BBjException, NoSuchMethodException, IllegalAccessException,
        InvocationTargetException {
      ReflectionUtils.nullifyControl(component);

      component.setChecked(true);
      component.setTextPosition(Position.LEFT);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, atLeast(1)).setSelected(true);
      verify(control, atLeast(1)).setHorizontalTextPosition(anyInt());
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    @DisplayName("adding/removing supported events")
    void addingRemovingSupportedEvents() {
      ComponentEventListener<CheckEvent> checkListener = event -> {
      };
      ComponentEventListener<UncheckEvent> uncheckListener = event -> {
      };
      ComponentEventListener<ToggleEvent> toggleListener = event -> {
      };

      component.onCheck(checkListener);
      component.onUncheck(uncheckListener);
      component.onToggle(toggleListener);


      assertEquals(1, component.getEventListeners(CheckEvent.class).size());
      assertEquals(1, component.getEventListeners(UncheckEvent.class).size());
      assertEquals(1, component.getEventListeners(ToggleEvent.class).size());

      component.removeCheckListener(checkListener);
      component.removeUncheckListener(uncheckListener);
      component.removeToggleListener(toggleListener);

      assertEquals(0, component.getEventListeners(CheckEvent.class).size());
      assertEquals(0, component.getEventListeners(UncheckEvent.class).size());
      assertEquals(0, component.getEventListeners(ToggleEvent.class).size());
    }
  }
}
