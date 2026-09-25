package com.webforj.component.window;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjChildWindow;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class PanelTest {

  private BBjWindow parentBbjWindow;
  private BBjChildWindow childBbjWindow;
  private Window parent;
  private Environment environment;

  @BeforeEach
  void setUp() throws BBjException {
    parentBbjWindow = mock(BBjWindow.class);
    childBbjWindow = mock(BBjChildWindow.class);
    parent = mock(Window.class);
    when(parent.getBbjWindow()).thenReturn(parentBbjWindow);
    when(parentBbjWindow.getAvailableControlID()).thenReturn(42);
    when(parentBbjWindow.addChildWindow(anyInt(), anyString(), any(byte[].class), anyInt()))
        .thenReturn(childBbjWindow);

    environment = mock(Environment.class);
    BBjSysGui sysGui = mock(BBjSysGui.class);
    when(environment.getSysGui()).thenReturn(sysGui);
    when(sysGui.getAvailableContext()).thenReturn(7);
  }

  @Test
  void shouldConstructEmptyAndWithComponents() {
    assertEquals(0, new Panel().getComponentCount());
    assertEquals(1, new Panel(new Panel()).getComponentCount());
  }

  @Test
  void shouldHaveNoBbjIdByDefault() {
    assertEquals(-1, new Panel().getBBjId());
  }

  @Test
  void shouldCreateTheChildWindowWithTheGivenBbjId() throws BBjException {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
      Panel panel = new Panel();
      panel.setBBjId(107);

      panel.onCreate(parent);

      verify(parentBbjWindow).addChildWindow(eq(107), eq(""), any(byte[].class), eq(7));
      verify(parentBbjWindow, never()).getAvailableControlID();
      assertEquals(childBbjWindow, panel.getBbjWindow());
    }
  }

  @Test
  void shouldCreateTheChildWindowWithTheNextAvailableIdWhenNoneIsSet() throws BBjException {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
      Panel panel = new Panel();

      panel.onCreate(parent);

      verify(parentBbjWindow).addChildWindow(eq(42), eq(""), any(byte[].class), eq(7));
    }
  }
}
