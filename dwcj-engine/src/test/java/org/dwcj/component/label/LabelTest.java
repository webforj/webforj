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
import org.dwcj.component.TextAlignable.Alignment;
import org.dwcj.component.event.EventController;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
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

/** Label tests. */
@ExtendWith(MockitoExtension.class)
public class LabelTest {

  @Mock
  BBjStaticText control;

  @Mock
  EventController<MouseEnterEvent> mouseEnterMock;

  @Mock
  EventController<MouseExitEvent> mouseExitMock;

  @Mock
  EventController<RightMouseDownEvent> rightMouseDownMock;

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
      componentSpy.setTextAlignment(Alignment.MIDDLE);
      componentSpy.onMouseEnter(e -> {
      });
      componentSpy.onMouseExit(e -> {
      });
      componentSpy.onRightMouseDown(e -> {
      });

      invokeCatchUp(componentSpy);

      verify(componentSpy, atLeast(2)).setWrap(false);
      verify(componentSpy, atLeast(2)).setTextAlignment(Alignment.MIDDLE);
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
      component.setTextAlignment(align);
      assertSame(component.getTextAlignment(), align);

      verify(control, times(1)).setAlignment(anyInt());
      verify(control, times(0)).getAlignment();
    }

    @ParameterizedTest
    @EnumSource(Alignment.class)
    @DisplayName("When control is null")
    void whenControlIsNull(Alignment align) throws BBjException, IllegalAccessException {
      nullifyControl();
      component.setTextAlignment(align);
      assertSame(component.getTextAlignment(), align);
    }

    @ParameterizedTest
    @EnumSource(Alignment.class)
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException(Alignment align) throws BBjException {
      doThrow(BBjException.class).when(control).setAlignment(anyInt());
      assertThrows(DwcjRuntimeException.class, () -> component.setTextAlignment(align));
    }
  }
}
