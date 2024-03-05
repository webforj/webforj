package com.webforj.component.window;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjWindow;
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
}
