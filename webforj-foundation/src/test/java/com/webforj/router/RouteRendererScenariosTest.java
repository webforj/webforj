package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
import com.webforj.Page;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.window.Frame;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class RouteRendererScenariosTest {

  MockedStatic<ConceiverProvider> mockedConceiverProvider;
  RouteRegistry routeRegistry;
  RouteRenderer routeRenderer;
  MockedStatic<Environment> mockedEnvironment;
  MockedStatic<Page> mockedPage;

  @BeforeEach
  void setUp() throws BBjException {
    mockedConceiverProvider = mockStatic(ConceiverProvider.class);
    when(ConceiverProvider.getCurrent()).thenReturn(new DefaultConceiver());

    routeRegistry = new RouteRegistry();
    routeRegistry.register("main", MainView.class); // /main
    routeRegistry.register("about", AboutView.class); // /about
    routeRegistry.register("main/submain", SubMainView.class, MainView.class); // /main/submain

    routeRenderer = spy(new RouteRenderer(routeRegistry));

    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(mock(Environment.class));
    BBjSysGui sysGui = mock(BBjSysGui.class);
    when(Environment.getCurrent().getSysGui()).thenReturn(sysGui);

    // Setup mock windows and their user data
    Frame f1 = mock(Frame.class);
    Frame f2 = mock(Frame.class);

    when(f1.getName()).thenReturn("frame1");
    when(f2.getName()).thenReturn("frame2");

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
    mockedConceiverProvider.close();
  }

  @Test
  void shouldNavigateToMain() {
    routeRenderer.render(MainView.class, c -> {
      assertNotNull(c);
      assertEquals(MainView.class, c.get().getClass());
    });
  }

  @Test
  void shouldNavigateFromMainToAbout() {
    routeRenderer.render(MainView.class, mainResult -> {
      assertTrue(mainResult.isPresent());
      MainView mainView = mainResult.get();
      assertEquals(MainView.class, mainView.getClass());

      routeRenderer.render(AboutView.class, aboutResult -> {
        assertNotNull(aboutResult);
        AboutView aboutView = aboutResult.get();
        assertEquals(AboutView.class, aboutView.getClass());
        assertFalse(mainView.isDestroyed());
      });
    });
  }

  @Test
  void shouldNavigateFromMainToSubMain() {
    routeRenderer.render(MainView.class, mainResult -> {
      assertNotNull(mainResult);
      MainView mainView = mainResult.get();
      assertEquals(MainView.class, mainView.getClass());

      routeRenderer.render(SubMainView.class, subMainResult -> {
        assertNotNull(subMainResult);
        SubMainView subMainView = subMainResult.get();
        assertEquals(SubMainView.class, subMainView.getClass());
        assertFalse(mainView.isDestroyed());
      });
    });
  }

  @Test
  void shouldNavigateFromSubMainToMain() {
    routeRenderer.render(SubMainView.class, subMainResult -> {
      assertNotNull(subMainResult);
      SubMainView subMainView = subMainResult.get();
      assertEquals(SubMainView.class, subMainView.getClass());

      routeRenderer.render(MainView.class, mainResult -> {
        assertNotNull(mainResult);
        MainView mainView = mainResult.get();
        assertEquals(MainView.class, mainView.getClass());
        assertTrue(subMainView.isDestroyed());
      });
    });
  }

  @NodeName("view-main")
  public static class MainView extends ElementCompositeContainer {
    // Mock implementation
  }

  @NodeName("view-about")
  public static class AboutView extends ElementCompositeContainer {
    // Mock implementation
  }

  @NodeName("view-submain")
  public static class SubMainView extends ElementCompositeContainer {
    // Mock implementation
  }
}
