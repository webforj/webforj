package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.Environment;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.window.Frame;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class RouteRendererScenariosTest {

  private RouteRegistry routeRegistry;
  private RouteRenderer routeRenderer;
  private MockedStatic<Environment> mockedEnvironment;

  @BeforeEach
  void setUp() throws BBjException {
    routeRegistry = new RouteRegistry();
    routeRegistry.register("main", MainView.class); // /main
    routeRegistry.register("about", AboutView.class); // /about
    routeRegistry.register("submain", SubMainView.class, MainView.class); // /main/submain

    routeRenderer = spy(new RouteRenderer(routeRegistry));

    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(mock(Environment.class));
    BBjSysGui sysGui = mock(BBjSysGui.class);
    when(Environment.getCurrent().getSysGui()).thenReturn(sysGui);

    // Setup mock windows and their user data
    Frame f1 = mock(Frame.class);
    Frame f2 = mock(Frame.class);

    when(f1.getFrameId()).thenReturn("frame1");
    when(f2.getFrameId()).thenReturn("frame2");

    BBjTopLevelWindow w1 = mock(BBjTopLevelWindow.class);
    BBjTopLevelWindow w2 = mock(BBjTopLevelWindow.class);
    when(w1.getUserData()).thenReturn(f1);
    when(w2.getUserData()).thenReturn(f2);

    // Mock the windows returned by sysGui
    when(sysGui.getWindows()).thenReturn(new BBjVector(List.of(w1, w2)));
  }

  @AfterEach
  public void teardown() {
    mockedEnvironment.close();
  }

  @Test
  void shouldNavigateToMain() {
    routeRenderer.navigate(MainView.class, c -> {
      assertTrue(c.isPresent());
      assertEquals(MainView.class, c.get().getClass());
    });
  }

  @Test
  void shouldNavigateFromMainToAbout() {
    routeRenderer.navigate(MainView.class, mainResult -> {
      assertTrue(mainResult.isPresent());
      MainView mainView = (MainView) mainResult.get();
      assertEquals(MainView.class, mainView.getClass());

      routeRenderer.navigate(AboutView.class, aboutResult -> {
        assertTrue(aboutResult.isPresent());
        AboutView aboutView = (AboutView) aboutResult.get();
        assertEquals(AboutView.class, aboutView.getClass());
        assertFalse(mainView.isDestroyed());
      });
    });
  }

  @Test
  void shouldNavigateFromMainToSubMain() {
    routeRenderer.navigate(MainView.class, mainResult -> {
      assertTrue(mainResult.isPresent());
      MainView mainView = (MainView) mainResult.get();
      assertEquals(MainView.class, mainView.getClass());

      routeRenderer.navigate(SubMainView.class, subMainResult -> {
        assertTrue(subMainResult.isPresent());
        SubMainView subMainView = (SubMainView) subMainResult.get();
        assertEquals(SubMainView.class, subMainView.getClass());
        assertFalse(mainView.isDestroyed());
      });
    });
  }

  @Test
  void shouldNavigateFromSubMainToMain() {
    routeRenderer.navigate(SubMainView.class, subMainResult -> {
      assertTrue(subMainResult.isPresent());
      SubMainView subMainView = (SubMainView) subMainResult.get();
      assertEquals(SubMainView.class, subMainView.getClass());

      routeRenderer.navigate(MainView.class, mainResult -> {
        assertTrue(mainResult.isPresent());
        MainView mainView = (MainView) mainResult.get();
        assertEquals(MainView.class, mainView.getClass());
        assertTrue(subMainView.isDestroyed());
      });
    });
  }

  @NodeName("view-main")
  static class MainView extends ElementCompositeContainer {
    // Mock implementation of MainView
  }

  @NodeName("view-about")
  static class AboutView extends ElementCompositeContainer {
    // Mock implementation of AboutView
  }

  @NodeName("view-submain")
  static class SubMainView extends ElementCompositeContainer {
    // Mock implementation of SubMainView
  }
}
