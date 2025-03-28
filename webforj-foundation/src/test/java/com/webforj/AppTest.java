package com.webforj;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjBusyIndicator;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.component.window.Frame;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class AppTest {

  Environment environment;
  BBjAPI api;
  BBjWebManager webManager;
  BBjSysGui sysGui;
  WebforjBBjBridge bridge;
  BBjBusyIndicator busyIndicator;
  App app;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    webManager = mock(BBjWebManager.class);
    sysGui = mock(BBjSysGui.class);
    bridge = mock(WebforjBBjBridge.class);
    busyIndicator = mock(BBjBusyIndicator.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(environment.getSysGui()).thenReturn(sysGui);
    when(environment.getBridge()).thenReturn(bridge);
    when(api.getWebManager()).thenReturn(webManager);
    when(webManager.getBusyIndicator()).thenReturn(busyIndicator);

    app = spy(App.class);
    when(app.getEnvironment()).thenReturn(environment);
  }

  @Nested
  class AppCloseActions {
    static Stream<Arguments> actionProvider() {
      return Stream.of(
          Arguments.of(new RedirectAction("https://example.com"), "urlAction",
              "https://example.com"),
          Arguments.of(new MessageAction("An error occurred."), "msgAction", "An error occurred."),
          Arguments.of(App.DEFAULT_ACTION, "defaultAction", null),
          Arguments.of(App.NONE_ACTION, "noneAction", null));
    }

    @ParameterizedTest
    @MethodSource("actionProvider")
    void testTerminateActions(AppCloseAction action, String expectedMethod, String expectedArgument)
        throws BBjException {
      // Test setTerminateAction
      app.setTerminateAction(action);
      verifyActionSet(expectedMethod, expectedArgument, true);
    }

    @ParameterizedTest
    @MethodSource("actionProvider")
    void testErrorActions(AppCloseAction action, String expectedMethod, String expectedArgument)
        throws BBjException {
      app.setErrorAction(action);
      verifyActionSet(expectedMethod, expectedArgument, false);
    }

    void verifyActionSet(String expectedMethod, String expectedArgument, boolean isTerminateAction)
        throws BBjException {
      switch (expectedMethod) {
        case "urlAction":
          if (isTerminateAction) {
            verify(webManager).setEndAction(webManager.urlAction(expectedArgument));
          } else {
            verify(webManager).setErrAction(webManager.urlAction(expectedArgument));
          }
          break;
        case "msgAction":
          if (isTerminateAction) {
            verify(webManager).setEndAction(webManager.msgAction(expectedArgument));
          } else {
            verify(webManager).setErrAction(webManager.msgAction(expectedArgument));
          }
          break;
        case "defaultAction":
          if (isTerminateAction) {
            verify(webManager).setEndAction(webManager.defaultAction());
          } else {
            verify(webManager).setErrAction(webManager.defaultAction());
          }
          break;
        case "noneAction":
          if (isTerminateAction) {
            verify(webManager).setEndAction(webManager.noneAction());
          } else {
            verify(webManager).setErrAction(webManager.noneAction());
          }
          break;
        default:
          throw new IllegalArgumentException("Unsupported action type.");
      }
    }
  }

  @Nested
  class BusyIndicator {

    @Test
    void shouldShowBusyIndicatorWithGivenMessage() {
      try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
        mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
        App.busy("pending the spoon");
        verify(busyIndicator).setText("pending the spoon");

        App.busy("pending the spoon", true);
        verify(busyIndicator).setHtml("pending the spoon");
      }
    }

    @Test
    void shouldHideShowBusyIndicator() {
      try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
        mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);

        App.busy(true);
        verify(busyIndicator).setVisible(true);

        App.busy(false);
        verify(busyIndicator).setVisible(false);
      }
    }
  }

  @Test
  void shouldReturnFrame() throws BBjException {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);

      Frame f1 = mock(Frame.class);
      Frame f2 = mock(Frame.class);

      BBjTopLevelWindow w1 = mock(BBjTopLevelWindow.class);
      when(w1.getUserData()).thenReturn(f1);
      BBjTopLevelWindow w2 = mock(BBjTopLevelWindow.class);
      when(w2.getUserData()).thenReturn(f2);

      when(sysGui.getWindows()).thenReturn(new BBjVector(List.of(w1, w2)));

      List<Frame> frames = App.getFrames();
      verify(sysGui).getWindows();

      assertTrue(frames.contains(f1));
      assertTrue(frames.contains(f2));
    }
  }
}
