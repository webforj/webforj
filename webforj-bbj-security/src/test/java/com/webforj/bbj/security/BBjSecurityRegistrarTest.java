package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.environment.ObjectTable;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

class BBjSecurityRegistrarTest {

  private MockedStatic<Environment> environmentMock;
  private MockedStatic<Router> routerMock;
  private MockedStatic<ObjectTable> tableMock;
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
    tableMock = mockStatic(ObjectTable.class);
    when(router.getRenderer()).thenReturn(renderer);
    registrar = new BBjSecurityRegistrar();
  }

  @AfterEach
  void tearDown() {
    tableMock.close();
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

  @Test
  void shouldRegisterTheServiceWhenEnabled() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));

    registrar.onWillRun(app);

    tableMock.verify(
        () -> ObjectTable.put(eq(BBjSecurity.SERVICE_KEY), any(BBjAuthenticationService.class)));
  }

  @Test
  void shouldRegisterNoServiceWhenDisabled() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = false"));

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
  }

  @Test
  void shouldRegisterNoServiceWithoutEnvironment() {
    environmentMock.when(Environment::getCurrent).thenReturn(null);

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
  }

  @Test
  void shouldRegisterNoServiceWithoutConfig() {
    Config missing = null;
    when(environment.getConfig()).thenReturn(missing);

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
  }

  @Test
  void shouldRegisterNoServiceWithoutRouter() {
    routerMock.when(Router::getCurrent).thenReturn(null);
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
  }

  @Test
  void shouldReadTheUserFromTheRegisteredService() {
    BBjPrincipal principal = new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of());
    List<BBjRouteSecurityContext> contexts = new ArrayList<>();
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());

    try (
        MockedConstruction<BBjAuthenticationService> services = mockConstruction(
            BBjAuthenticationService.class,
            (service, context) -> when(service.currentUser()).thenReturn(Optional.of(principal)));
        MockedConstruction<BBjRouteSecurityManager> managers =
            mockConstruction(BBjRouteSecurityManager.class, (manager, context) -> contexts
                .add((BBjRouteSecurityContext) context.arguments().get(1)))) {
      registrar.onWillRun(app);

      assertEquals(1, contexts.size());
      assertTrue(contexts.get(0).isAuthenticated());
      assertEquals(Optional.of(principal), contexts.get(0).getPrincipal());
      BBjAuthenticationService registered = services.constructed().get(0);
      tableMock.verify(() -> ObjectTable.put(BBjSecurity.SERVICE_KEY, registered));
    }
  }
}
