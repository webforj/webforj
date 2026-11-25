package com.webforj.component.window;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.function.Supplier;
import org.example.accessor.ForeignAccessorCaller;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class WindowAccessorImplTest {
  WindowAccessorImpl windowAccessor;

  @BeforeEach
  void setUp() {
    windowAccessor = new WindowAccessorImpl();
  }

  @Test
  @DisplayName("Should get BBjWindow")
  void shouldGetBbjWindow() throws IllegalAccessException {
    Window component = spy(Window.class);
    BBjWindow mockedBbjWindow = mock(BBjWindow.class);
    when(component.getBbjWindow()).thenReturn(mockedBbjWindow);

    BBjWindow underlyingWindow = windowAccessor.getBBjWindow(component);
    ((Window) verify(component)).getBbjWindow();
    assertEquals(mockedBbjWindow, underlyingWindow);
  }

  @Test
  void shouldRefuseCallerOutsideWebforjPackage() {
    Window window = mock(Window.class);

    IllegalAccessException exception = assertThrows(IllegalAccessException.class,
        () -> ForeignAccessorCaller.getBBjWindow(windowAccessor, window));

    assertEquals("The class 'org.example.accessor.ForeignAccessorCaller' attempted unauthorized "
        + "access to BBj Window. Access is restricted to classes within the 'com.webforj' package "
        + "hierarchy. Ensure that any class interacting with BBj Window adheres to this access "
        + "control policy.", exception.getMessage());
  }

  @Test
  void shouldRefuseReflectiveCall() throws NoSuchMethodException {
    Window window = mock(Window.class);
    Method getBBjWindow = WindowAccessorImpl.class.getMethod("getBBjWindow", Window.class);

    InvocationTargetException exception = assertThrows(InvocationTargetException.class,
        () -> getBBjWindow.invoke(windowAccessor, window));

    assertTrue(exception.getCause() instanceof IllegalAccessException);
  }

  @Test
  void shouldAllowCallFromLambdaInWebforjPackage() throws IllegalAccessException {
    Window window = spy(Window.class);
    BBjWindow mockedBbjWindow = mock(BBjWindow.class);
    when(window.getBbjWindow()).thenReturn(mockedBbjWindow);
    Supplier<BBjWindow> supplier = () -> {
      try {
        return windowAccessor.getBBjWindow(window);
      } catch (IllegalAccessException e) {
        throw new IllegalStateException(e);
      }
    };

    assertEquals(mockedBbjWindow, supplier.get());
  }
}
