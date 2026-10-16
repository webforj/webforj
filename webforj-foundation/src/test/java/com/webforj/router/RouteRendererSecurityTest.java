package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.Environment;
import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Window;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityManager;
import com.webforj.router.security.RouteSecurityObserver;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class RouteRendererSecurityTest {

  MockedStatic<ConceiverProvider> mockedConceiverProvider;
  RouteRegistry routeRegistry;
  RouteRenderer routeRenderer;
  MockedStatic<Environment> mockedEnvironment;
  RouteSecurityManager securityManager;

  @BeforeEach
  void setUp() throws BBjException {
    mockedConceiverProvider = mockStatic(ConceiverProvider.class);
    when(ConceiverProvider.getCurrent()).thenReturn(new DefaultConceiver());

    routeRegistry = new RouteRegistry();
    routeRenderer = spy(new RouteRenderer(routeRegistry));

    mockedEnvironment = mockStatic(Environment.class);
    Environment environment = mock(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
    BBjSysGui sysGui = mock(BBjSysGui.class);
    when(Environment.getCurrent().getSysGui()).thenReturn(sysGui);

    Frame f1 = mock(Frame.class);
    Frame f2 = mock(Frame.class);

    when(f1.getName()).thenReturn("frame1");
    when(f2.getName()).thenReturn("frame2");

    BBjTopLevelWindow w1 = mock(BBjTopLevelWindow.class);
    BBjTopLevelWindow w2 = mock(BBjTopLevelWindow.class);
    when(w1.getUserData()).thenReturn(f1);
    when(w2.getUserData()).thenReturn(f2);

    when(sysGui.getWindows()).thenReturn(new BBjVector(List.of(w1, w2)));

    GuardedView.constructorCalls.set(0);
    ThrowingGuardedView.constructorCalls.set(0);
    securityManager = mock(RouteSecurityManager.class);
    when(securityManager.evaluate(any(), any())).thenReturn(RouteAccessDecision.grant());
    routeRenderer.addObserver(new RouteSecurityObserver(securityManager));
  }

  @AfterEach
  void teardown() {
    mockedEnvironment.close();
    mockedConceiverProvider.close();
  }

  @Test
  void shouldNotConstructDeniedRoute() {
    routeRegistry.register("/guarded", GuardedView.class);
    RouteAccessDecision denial = RouteAccessDecision.denyAuthentication("Login required");
    when(securityManager.evaluate(eq(GuardedView.class), any())).thenReturn(denial);

    AtomicReference<Optional<GuardedView>> rendered = new AtomicReference<>();
    routeRenderer.render(GuardedView.class, new NavigationContext(), rendered::set);

    assertEquals(0, GuardedView.constructorCalls.get());
    assertFalse(rendered.get().isPresent());
    verify(securityManager).onAccessDenied(eq(denial), any());
  }

  @Test
  void shouldCompleteWithDenialWhenDeniedRouteConstructorThrows() {
    routeRegistry.register("/guarded", ThrowingGuardedView.class);
    RouteAccessDecision denial = RouteAccessDecision.denyAuthentication("Login required");
    when(securityManager.evaluate(eq(ThrowingGuardedView.class), any())).thenReturn(denial);

    AtomicReference<Optional<ThrowingGuardedView>> rendered = new AtomicReference<>();
    assertDoesNotThrow(() -> routeRenderer.render(ThrowingGuardedView.class,
        new NavigationContext(), rendered::set));

    assertFalse(rendered.get().isPresent());
    verify(securityManager).onAccessDenied(eq(denial), any());
  }

  @Test
  void shouldConstructGrantedRouteOnce() {
    routeRegistry.register("/guarded", GuardedView.class);

    NavigationContext context = new NavigationContext();
    AtomicReference<Optional<GuardedView>> rendered = new AtomicReference<>();
    routeRenderer.render(GuardedView.class, context, rendered::set);

    assertEquals(1, GuardedView.constructorCalls.get());
    assertTrue(rendered.get().isPresent());
    verify(securityManager).evaluate(GuardedView.class, context);
    verify(securityManager, never()).onAccessDenied(any(), any());
  }

  @Test
  void shouldNotConstructDeniedRouteInsideAllowedOutlet() {
    routeRegistry.register("/home", HomeView.class);
    routeRegistry.register("/guarded", GuardedView.class, HomeView.class);
    RouteAccessDecision denial = RouteAccessDecision.denyAuthentication("Login required");
    when(securityManager.evaluate(eq(GuardedView.class), any())).thenReturn(denial);

    AtomicReference<Optional<GuardedView>> rendered = new AtomicReference<>();
    routeRenderer.render(GuardedView.class, new NavigationContext(), rendered::set);

    assertEquals(0, GuardedView.constructorCalls.get());
    assertFalse(rendered.get().isPresent());
    verify(securityManager).onAccessDenied(eq(denial), any());
  }

  public static final class HomeView extends Component implements RouteOutlet {
    @Override
    protected void onCreate(Window window) {}

    @Override
    protected void onDestroy() {}

    @Override
    public void showRouteContent(Component component) {}

    @Override
    public void removeRouteContent(Component component) {}
  }

  public static final class GuardedView extends Component {
    static final AtomicInteger constructorCalls = new AtomicInteger();

    public GuardedView() {
      constructorCalls.incrementAndGet();
    }

    @Override
    protected void onCreate(Window window) {}

    @Override
    protected void onDestroy() {}
  }

  public static final class ThrowingGuardedView extends Component {
    static final AtomicInteger constructorCalls = new AtomicInteger();

    public ThrowingGuardedView() {
      constructorCalls.incrementAndGet();
      throw new IllegalStateException("Secured service called without a session");
    }

    @Override
    protected void onCreate(Window window) {}

    @Override
    protected void onDestroy() {}
  }
}
