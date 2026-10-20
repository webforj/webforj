package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.environment.ObjectTable;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class BBjRouteSecurityRegistrarTest {
  private final BBjRouteSecurityRegistrar registrar = new BBjRouteSecurityRegistrar();
  private final App app = mock(App.class);
  private final Environment environment = mock(Environment.class);
  private final Router router = mock(Router.class);
  private final RouteRenderer renderer = mock(RouteRenderer.class);
  private MockedStatic<Environment> mockedEnvironment;
  private MockedStatic<Router> mockedRouter;
  private MockedStatic<ObjectTable> mockedObjectTable;

  @BeforeEach
  void setUp() {
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());
    when(router.getRenderer()).thenReturn(renderer);
    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
    mockedRouter = mockStatic(Router.class);
    mockedRouter.when(Router::getCurrent).thenReturn(router);
    mockedObjectTable = mockStatic(ObjectTable.class);
  }

  @AfterEach
  void tearDown() {
    mockedObjectTable.close();
    mockedRouter.close();
    mockedEnvironment.close();
  }

  @Test
  void shouldRegisterTheSecurityObserverWhenEnabled() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));
    registrar.onWillRun(app);

    verify(renderer).addObserver(any(RouteSecurityObserver.class));
  }

  @Test
  void shouldRegisterTheSecurityContextForTheApp() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));

    registrar.onWillRun(app);

    mockedObjectTable.verify(() -> ObjectTable.put(eq(BBjRouteSecurityContext.class.getName()),
        any(BBjRouteSecurityContext.class)));
  }

  @Test
  void shouldNotRegisterWhenDisabledByConfiguration() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = false"));

    registrar.onWillRun(app);

    verify(renderer, never()).addObserver(any());
    mockedObjectTable.verifyNoInteractions();
  }

  @Test
  void shouldNotRegisterOrThrowWhenTheEnabledKeyIsMissing() {
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString("webforj.other = 1"));

    assertDoesNotThrow(() -> registrar.onWillRun(app));

    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldRegisterWhenTheEnabledValueIsInvalid() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = maybe"));

    assertDoesNotThrow(() -> registrar.onWillRun(app));

    verify(renderer).addObserver(any(RouteSecurityObserver.class));
  }

  @Test
  void shouldNotRegisterWithoutAnEnvironment() {
    mockedEnvironment.when(Environment::getCurrent).thenReturn(null);

    assertDoesNotThrow(() -> registrar.onWillRun(app));

    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldNotRegisterWithoutRouter() {
    mockedRouter.when(Router::getCurrent).thenReturn(null);

    assertDoesNotThrow(() -> registrar.onWillRun(app));

    verify(renderer, never()).addObserver(any());
  }
}
