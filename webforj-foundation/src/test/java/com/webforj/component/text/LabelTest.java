package com.webforj.component.text;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class LabelTest {

  @Mock
  BBjStaticText control;

  @Spy
  EventDispatcher dispatcher;

  @InjectMocks
  Label component;

  @Nested
  @DisplayName("Constructor")
  class Constructors {

    @Test
    @DisplayName("Constructor with text and wrap")
    void textAndWrapConstructor() {
      Label localComponent = new Label("text", false);
      assertEquals("text", localComponent.getText());
      assertFalse(localComponent.isWrap());
    }
  }

  @Nested
  @DisplayName("OnAttach")
  class OnAttach {

    @Test
    @DisplayName("OnAttach method")
    void catchup() throws BBjException, IllegalAccessException {

      ReflectionUtils.nullifyControl(component);
      component.setWrap(false);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setLineWrap(false);
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
    void whenControlIsNull() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setWrap(false);
      assertFalse(component.isWrap());
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setLineWrap(anyBoolean());
      assertThrows(WebforjRuntimeException.class, () -> component.setWrap(false));
    }
  }
}
