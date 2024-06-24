package com.webforj.component.optioninput;

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
import com.webforj.component.ReflectionUtils;
import com.webforj.component.event.CheckEvent;
import com.webforj.component.event.ToggleEvent;
import com.webforj.component.event.UncheckEvent;
import com.webforj.concern.HasTextPosition.Position;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.lang.reflect.InvocationTargetException;
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

      component.setValue(true);
      assertTrue(component.getValue());

      verify(control, times(1)).setSelected(true);
      verify(control, times(1)).isSelected();
    }

    @Test
    @DisplayName("When Control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setValue(true);
      assertTrue(component.getValue());

      verify(control, times(0)).setSelected(true);
      verify(control, times(0)).isSelected();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setSelected(anyBoolean());
      assertThrows(WebforjRuntimeException.class, () -> component.setChecked(true));

      doThrow(BBjException.class).when(control).isSelected();
      assertThrows(WebforjRuntimeException.class, () -> component.isChecked());
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
      assertThrows(WebforjRuntimeException.class, () -> component.setTextPosition(Position.LEFT));
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
      EventListener<CheckEvent> checkListener = event -> {
      };
      EventListener<UncheckEvent> uncheckListener = event -> {
      };
      EventListener<ToggleEvent> toggleListener = event -> {
      };

      ListenerRegistration<CheckEvent> r1 = component.onCheck(checkListener);
      ListenerRegistration<UncheckEvent> r2 = component.onUncheck(uncheckListener);
      ListenerRegistration<ToggleEvent> r3 = component.onToggle(toggleListener);


      assertEquals(1, component.getEventListeners(CheckEvent.class).size());
      assertEquals(1, component.getEventListeners(UncheckEvent.class).size());
      assertEquals(1, component.getEventListeners(ToggleEvent.class).size());

      r1.remove();
      r2.remove();
      r3.remove();

      assertEquals(0, component.getEventListeners(CheckEvent.class).size());
      assertEquals(0, component.getEventListeners(UncheckEvent.class).size());
      assertEquals(0, component.getEventListeners(ToggleEvent.class).size());
    }

    @Test
    void shouldConfigureValueChangeEvent() {
      ListenerRegistration<ValueChangeEvent<Boolean>> r1 = component.onValueChange(e -> {
      });
      assertEquals(1, component.getEventListeners(ToggleEvent.class).size());
    }
  }

  @Test
  void shouldSetGetHelperText() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setHelperText("helper text");
    assertEquals("helper text", component.getHelperText());

    assertEquals("helper text", component.getProperty("helperText"));
  }
}
