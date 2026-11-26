package com.webforj.component.toast;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjObjectTable;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Theme;
import com.webforj.component.element.Element;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasTheme;
import java.util.Collections;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class ToastTest {

  Toast component;

  @BeforeEach
  void setUp() {
    component = new Toast();
  }

  @Test
  void shouldDeclareTheThemeConcern() {
    assertInstanceOf(HasTheme.class, component);
  }

  @Nested
  class Constructors {
    Frame mockFrame;

    @BeforeEach
    void setUp() {
      mockFrame = mock(Frame.class);
    }

    @Test
    void shouldCreateWithTextDurationThemeAndPlacement() {
      try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
        when(App.getFrames()).thenReturn(Collections.singletonList(mockFrame));

        component = Toast.show("Hello, World!", 5000, Theme.INFO, Toast.Placement.TOP_RIGHT);
        assertEquals("Hello, World!", component.getText());
        assertEquals(5000, component.getDuration());
        assertEquals(Theme.INFO, component.getTheme());
        assertEquals(Toast.Placement.TOP_RIGHT, component.getPlacement());
      }
    }

    @Test
    void shouldCreateWithTextDurationAndTheme() {
      try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
        when(App.getFrames()).thenReturn(Collections.singletonList(mockFrame));
        component = Toast.show("Hello, World!", 5000, Theme.INFO);

        assertEquals("Hello, World!", component.getText());
        assertEquals(5000, component.getDuration());
        assertEquals(Theme.INFO, component.getTheme());
        assertEquals(Toast.Placement.BOTTOM, component.getPlacement());
      }
    }

    @Test
    void shouldCreateWithTextAndTheme() {
      try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
        when(App.getFrames()).thenReturn(Collections.singletonList(mockFrame));
        component = Toast.show("Hello, World!", Theme.INFO);

        assertEquals("Hello, World!", component.getText());
        assertEquals(Toast.DEFAULT_DURATION, component.getDuration());
        assertEquals(Theme.INFO, component.getTheme());
        assertEquals(Toast.Placement.BOTTOM, component.getPlacement());
      }
    }

    @Test
    void shouldCreateWithTextDurationAndPlacement() {
      try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
        when(App.getFrames()).thenReturn(Collections.singletonList(mockFrame));
        component = Toast.show("Hello, World!", 5000, Toast.Placement.TOP_RIGHT);

        assertEquals("Hello, World!", component.getText());
        assertEquals(5000, component.getDuration());
        assertEquals(Theme.DEFAULT, component.getTheme());
        assertEquals(Toast.Placement.TOP_RIGHT, component.getPlacement());
      }
    }

    @Test
    void shouldCreateWithTextAndPlacement() {
      try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
        when(App.getFrames()).thenReturn(Collections.singletonList(mockFrame));
        component = Toast.show("Hello, World!", Toast.Placement.TOP_RIGHT);

        assertEquals("Hello, World!", component.getText());
        assertEquals(Toast.DEFAULT_DURATION, component.getDuration());
        assertEquals(Theme.DEFAULT, component.getTheme());
        assertEquals(Toast.Placement.TOP_RIGHT, component.getPlacement());
      }
    }

    @Test
    void shouldCreateWithTextAndDuration() {
      try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
        when(App.getFrames()).thenReturn(Collections.singletonList(mockFrame));
        component = Toast.show("Hello, World!", 5000);

        assertEquals("Hello, World!", component.getText());
        assertEquals(5000, component.getDuration());
        assertEquals(Theme.DEFAULT, component.getTheme());
        assertEquals(Toast.Placement.BOTTOM, component.getPlacement());
      }
    }

    @Test
    void shouldCreateWithText() {
      try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
        when(App.getFrames()).thenReturn(Collections.singletonList(mockFrame));
        component = Toast.show("Hello, World!");

        assertEquals("Hello, World!", component.getText());
        assertEquals(Toast.DEFAULT_DURATION, component.getDuration());
        assertEquals(Theme.DEFAULT, component.getTheme());
        assertEquals(Toast.Placement.BOTTOM, component.getPlacement());
      }
    }

    @Test
    void shouldCreateWithDefaultValues() {
      assertEquals("", component.getText());
      assertEquals(Toast.DEFAULT_DURATION, component.getDuration());
      assertEquals(Theme.DEFAULT, component.getTheme());
      assertEquals(Toast.Placement.BOTTOM, component.getPlacement());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      assertDoesNotThrow(() -> {
        PropertyDescriptorTester.run(Toast.class, component);
      });
    }

    @Test
    void shouldShowTextLiterallyAndExtractTextFromHtml() {
      component.setText("<div>Hello, World!</div>");
      assertEquals("<div>Hello, World!</div>", component.getText());

      component.setHtml("<div>Hello, World!</div>");
      assertEquals("<div>Hello, World!</div>", component.getHtml());
      assertEquals("Hello, World!", component.getText());
    }
  }

  @Test
  void shouldCreateFrameIfNotExists() throws BBjException {
    Environment environment = mock(Environment.class);
    BBjAPI api = mock(BBjAPI.class);
    BBjObjectTable objectTable = mock(BBjObjectTable.class);
    BBjSysGui sysGui = mock(BBjSysGui.class);
    BBjTopLevelWindow mockWindow = mock(BBjTopLevelWindow.class); // Create a mock for BBjWindow

    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
      when(environment.getBBjAPI()).thenReturn(api);
      when(api.getObjectTable()).thenReturn(objectTable);
      when(environment.getSysGui()).thenReturn(sysGui);
      when(sysGui.addWindow(any(), anyString(), any(byte[].class))).thenReturn(mockWindow);

      try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
        when(App.getFrames()).thenReturn(Collections.emptyList());
        component = Toast.show("Hello, World!");
        assertTrue(component.isAttached());

        verify(mockWindow).setVisible(false);
      }
    }
  }

  @Test
  void shouldRemoveItsFrameObserverWhenDestroyed() {
    Frame frame = mock(Frame.class);
    ArgumentCaptor<ComponentLifecycleObserver> observer =
        ArgumentCaptor.forClass(ComponentLifecycleObserver.class);

    try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
      when(App.getFrames()).thenReturn(Collections.singletonList(frame));
      component = Toast.show("Hello, World!");
    }

    verify(frame).addLifecycleObserver(observer.capture());
    component.close();

    verify(frame).removeLifecycleObserver(observer.getValue());
  }

  @Test
  void shouldDestroyItselfWhenTheClientReportsItClosed() {
    Frame frame = mock(Frame.class);
    ArgumentCaptor<ComponentLifecycleObserver> observer =
        ArgumentCaptor.forClass(ComponentLifecycleObserver.class);

    try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
      when(App.getFrames()).thenReturn(Collections.singletonList(frame));
      component = Toast.show("Hello, World!");
    }

    verify(frame).addLifecycleObserver(observer.capture());
    Element element = component.getOriginalElement();
    ComponentUtil.fireComponentEvent(element,
        new ElementEvent(element, Map.of(), "dwc-closed", 0, null));

    assertTrue(component.isDestroyed());
    verify(frame).removeLifecycleObserver(observer.getValue());
  }

  @Test
  void shouldKeepAConstructedToastWhenTheClientReportsItClosed() {
    Frame frame = mock(Frame.class);
    component = new Toast("Hello, World!");

    try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
      when(App.getFrames()).thenReturn(Collections.singletonList(frame));
      component.open();
    }

    Element element = component.getOriginalElement();
    ComponentUtil.fireComponentEvent(element,
        new ElementEvent(element, Map.of(), "dwc-closed", 0, null));

    assertFalse(component.isDestroyed());
    verify(frame, never()).removeLifecycleObserver(any());
  }

  @Test
  void shouldIgnoreTheClosedReportOfADestroyedToast() {
    Frame frame = mock(Frame.class);

    try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
      when(App.getFrames()).thenReturn(Collections.singletonList(frame));
      component = Toast.show("Hello, World!");
    }

    Element element = component.getOriginalElement();
    component.close();

    assertDoesNotThrow(() -> ComponentUtil.fireComponentEvent(element,
        new ElementEvent(element, Map.of(), "dwc-closed", 0, null)));
    assertTrue(component.isDestroyed());
  }

  @Test
  void shouldKeepItsFrameObserverWhileOpen() {
    Frame frame = mock(Frame.class);

    try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
      when(App.getFrames()).thenReturn(Collections.singletonList(frame));
      component = Toast.show("Hello, World!");
    }

    verify(frame, never()).removeLifecycleObserver(any());
  }
}
