package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class BBjSecurityRegistrarTest {

  private MockedStatic<Environment> environmentMock;
  private MockedStatic<Router> routerMock;
  private Environment environment;
  private Router router;
  private RouteRenderer renderer;
  private App app;
  private BBjSecurityRegistrar registrar;

  @BeforeEach
  void setUp() {
    environment = mock(Environment.class);
    router = mock(Router.class);
    renderer = mock(RouteRenderer.class);
    app = mock(App.class);
    environmentMock = mockStatic(Environment.class);
    environmentMock.when(Environment::getCurrent).thenReturn(environment);
    routerMock = mockStatic(Router.class);
    routerMock.when(Router::getCurrent).thenReturn(router);
    when(router.getRenderer()).thenReturn(renderer);
    registrar = new BBjSecurityRegistrar();
  }

  @AfterEach
  void tearDown() {
    routerMock.close();
    environmentMock.close();
  }

  @Test
  void shouldAddTheSecurityObserverWhenEnabled() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));

    registrar.onWillRun(app);

    verify(renderer).addObserver(any(RouteSecurityObserver.class));
  }

  @Test
  void shouldAddTheSecurityObserverWhenTheSecuritySettingsAreAbsent() {
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());

    registrar.onWillRun(app);

    verify(renderer).addObserver(any(RouteSecurityObserver.class));
  }

  @Test
  void shouldAddNoObserverWhenDisabled() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = false"));

    registrar.onWillRun(app);

    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldAddNoObserverWithoutEnvironment() {
    environmentMock.when(Environment::getCurrent).thenReturn(null);

    assertDoesNotThrow(() -> registrar.onWillRun(app));

    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldAddNoObserverWithoutConfig() {
    Config missing = null;
    when(environment.getConfig()).thenReturn(missing);

    assertDoesNotThrow(() -> registrar.onWillRun(app));

    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldNotFailWithoutRouter() {
    routerMock.when(Router::getCurrent).thenReturn(null);
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());

    assertDoesNotThrow(() -> registrar.onWillRun(app));

    verify(renderer, never()).addObserver(any());
  }
}
