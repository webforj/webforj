package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.bbj.security.fixture.CapturedLog;
import com.webforj.bbj.security.fixture.FirstOwnerEvaluator;
import com.webforj.bbj.security.fixture.LateOwnerEvaluator;
import com.webforj.environment.ObjectTable;
import com.webforj.router.NavigationContext;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.RouteSecurityObserver;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.AuthenticationRequiredEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import com.webforj.webstorage.SessionStorage;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.ServiceConfigurationError;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

class BBjRouteSecurityRegistrarTest {
  private final BBjRouteSecurityRegistrar registrar = new BBjRouteSecurityRegistrar();
  private final App app = mock(App.class);
  private final Environment environment = mock(Environment.class);
  private final Router router = mock(Router.class);
  private final RouteRenderer renderer = mock(RouteRenderer.class);
  private MockedStatic<Environment> mockedEnvironment;
  private MockedStatic<Router> mockedRouter;
  private final ClassLoader contextClassLoader = Thread.currentThread().getContextClassLoader();
  private MockedStatic<ObjectTable> mockedObjectTable;
  private final List<Handler> handlers = new ArrayList<>();

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
    Thread.currentThread().setContextClassLoader(contextClassLoader);
    handlers.forEach(handler -> Logger.getLogger(BBjRouteSecurityRegistrar.class.getPackageName())
        .removeHandler(handler));
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
  void shouldKeepTheRequestedLocationInTheBrowserSessionStorage() {
    SessionStorage storage = mock(SessionStorage.class);
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));
    NavigationContext context = mock(NavigationContext.class);
    when(context.getLocation()).thenReturn(new Location("/protected"));

    try (MockedStatic<SessionStorage> mockedStorage = mockStatic(SessionStorage.class)) {
      mockedStorage.when(SessionStorage::getCurrent).thenReturn(storage);
      registrar.onWillRun(app);
      ArgumentCaptor<RouteSecurityObserver> observer =
          ArgumentCaptor.forClass(RouteSecurityObserver.class);
      verify(renderer).addObserver(observer.capture());
      mockedStorage.verifyNoInteractions();

      observer.getValue().getSecurityManager()
          .onAccessDenied(RouteAccessDecision.denyAuthentication(), context);
    }

    verify(storage).setItem(AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY, "/protected");
  }

  @Test
  void shouldNotTouchTheSessionStorageWithoutAnEnvironmentWhenSavingTheLocation() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));
    NavigationContext context = mock(NavigationContext.class);
    when(context.getLocation()).thenReturn(new Location("/protected"));

    try (MockedStatic<SessionStorage> mockedStorage = mockStatic(SessionStorage.class)) {
      registrar.onWillRun(app);
      ArgumentCaptor<RouteSecurityObserver> observer =
          ArgumentCaptor.forClass(RouteSecurityObserver.class);
      verify(renderer).addObserver(observer.capture());
      mockedEnvironment.when(Environment::getCurrent).thenReturn(null);

      assertDoesNotThrow(() -> observer.getValue().getSecurityManager()
          .onAccessDenied(RouteAccessDecision.denyAuthentication(), context));
      mockedStorage.verifyNoInteractions();
    }
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

  @Test
  void shouldRegisterTheCustomEvaluatorsWithTheirPriorities() throws IOException {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));

    try (URLClassLoader loader = createFixtureLoader("prioritized")) {
      Thread.currentThread().setContextClassLoader(loader);
      registrar.onWillRun(app);
    }

    ArgumentCaptor<RouteSecurityObserver> observer =
        ArgumentCaptor.forClass(RouteSecurityObserver.class);
    verify(renderer).addObserver(observer.capture());
    List<RouteSecurityEvaluator> evaluators =
        observer.getValue().getSecurityManager().getEvaluators();
    assertEquals(9, evaluators.size());
    assertEquals(FirstOwnerEvaluator.class, evaluators.get(0).getClass());
    assertEquals(
        List.of(DenyAllEvaluator.class, AnonymousAccessEvaluator.class,
            AuthenticationRequiredEvaluator.class, PermitAllEvaluator.class,
            RolesAllowedEvaluator.class),
        evaluators.subList(1, 6).stream().<Class<?>>map(RouteSecurityEvaluator::getClass).toList());
    assertEquals(List.of("zulu", "alpha"), evaluators.subList(6, 8).stream()
        .map(evaluator -> evaluator.evaluate(String.class, null, null, null).getReason()).toList());
    assertEquals(LateOwnerEvaluator.class, evaluators.get(8).getClass());
  }

  @Test
  void shouldNotRegisterWhenCustomEvaluatorFailsToLoad() throws IOException {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));

    try (URLClassLoader loader = createFixtureLoader("broken")) {
      Thread.currentThread().setContextClassLoader(loader);
      assertThrows(ServiceConfigurationError.class, () -> registrar.onWillRun(app));
    }

    verify(renderer, never()).addObserver(any());
    mockedObjectTable.verifyNoInteractions();
  }

  @Test
  void shouldNotLoadCustomEvaluatorsWhenDisabledByConfiguration() throws IOException {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = false"));

    try (URLClassLoader loader = createFixtureLoader("broken")) {
      Thread.currentThread().setContextClassLoader(loader);
      assertDoesNotThrow(() -> registrar.onWillRun(app));
    }

    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldStopTheStartWhenTheRendererIsNotAvailable() {
    enableSecurity();
    RuntimeException failure = new IllegalStateException("no renderer");
    when(router.getRenderer()).thenThrow(failure);

    CapturingHandler handler = captureLog();
    BBjSecurityError error = assertThrows(BBjSecurityError.class, () -> registrar.onWillRun(app));

    assertSame(failure, error.getCause());
    assertEquals(1, handler.getErrors().size());
    assertTrue(handler.getErrors().get(0).getMessage().contains("could not be registered"));
    assertSame(failure, handler.getErrors().get(0).getThrown());
  }

  @Test
  void shouldStopTheStartWhenTheObserverCannotBeAdded() {
    enableSecurity();
    RuntimeException failure = new IllegalStateException("no observer");
    doThrow(failure).when(renderer).addObserver(any());

    CapturingHandler handler = captureLog();
    BBjSecurityError error = assertThrows(BBjSecurityError.class, () -> registrar.onWillRun(app));

    assertSame(failure, error.getCause());
    assertEquals(1, handler.getErrors().size());
  }

  @Test
  void shouldStopTheStartWhenTheSecurityContextCannotBeStored() {
    enableSecurity();
    RuntimeException failure = new IllegalStateException("no table");
    mockedObjectTable.when(() -> ObjectTable.put(any(), any())).thenThrow(failure);

    CapturingHandler handler = captureLog();
    BBjSecurityError error = assertThrows(BBjSecurityError.class, () -> registrar.onWillRun(app));

    assertSame(failure, error.getCause());
    assertEquals(1, handler.getErrors().size());
    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldStopTheStartWhenTheManagerCannotBeCreated() {
    enableSecurity();

    try (MockedConstruction<BBjRouteSecurityManager> ignored =
        mockConstruction(BBjRouteSecurityManager.class, (manager, context) -> {
          throw new IllegalStateException("no manager");
        })) {
      CapturingHandler handler = captureLog();
      BBjSecurityError error = assertThrows(BBjSecurityError.class, () -> registrar.onWillRun(app));

      assertInstanceOf(RuntimeException.class, error.getCause());
      assertEquals(1, handler.getErrors().size());
    }

    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldStopTheStartWhenAnEvaluatorCannotBeRegistered() {
    enableSecurity();
    RuntimeException failure = new IllegalStateException("no priority");
    BBjRouteSecurityEvaluator evaluator = mock(BBjRouteSecurityEvaluator.class);
    when(evaluator.getPriority()).thenThrow(failure);

    try (MockedStatic<BBjRouteSecurityEvaluatorLoader> loader =
        mockStatic(BBjRouteSecurityEvaluatorLoader.class)) {
      loader.when(() -> BBjRouteSecurityEvaluatorLoader.loadEvaluators(any()))
          .thenReturn(List.of(evaluator));
      CapturingHandler handler = captureLog();
      BBjSecurityError error = assertThrows(BBjSecurityError.class, () -> registrar.onWillRun(app));

      assertSame(failure, error.getCause());
      assertEquals(1, handler.getErrors().size());
    }

    verify(renderer, never()).addObserver(any());
  }

  @Test
  void shouldLetAnErrorOfTheEvaluatorLoaderPassWithoutLoggingItAgain() throws IOException {
    enableSecurity();

    try (URLClassLoader loader = createFixtureLoader("broken")) {
      Thread.currentThread().setContextClassLoader(loader);
      CapturingHandler handler = captureLog();
      ServiceConfigurationError error =
          assertThrows(ServiceConfigurationError.class, () -> registrar.onWillRun(app));

      assertEquals(1, handler.getErrors().size());
      assertEquals(ServiceConfigurationError.class, error.getClass());
    }
  }

  @Test
  void shouldLogTheAdminConnectionWithoutSecretsWhenEnabled() {
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString("""
        webforj.security.enabled = true
        webforj.security.bbj.admin.host = localhost
        webforj.security.bbj.admin.port = 2003
        webforj.security.bbj.admin.truststore-password = registrarSecretValue
        """));

    try (CapturedLog log = new CapturedLog(BBjRouteSecurityRegistrar.class.getPackageName())) {
      registrar.onWillRun(app);

      assertEquals(
          List.of("BBj admin connection localhost:2003 ssl=false truststore=none timeout=10s"
              + " token=30min"),
          log.getMessages(Level.INFO).stream()
              .filter(message -> message.startsWith("BBj admin connection")).toList());
      assertTrue(log.getAllMessages().stream()
          .noneMatch(message -> message.contains("registrarSecretValue")));
    }

    verify(renderer).addObserver(any(RouteSecurityObserver.class));
  }

  @Test
  void shouldStopTheStartWhenConnectionKeyOfTheAdminConfigurationIsInvalid() {
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString("""
        webforj.security.enabled = true
        webforj.security.bbj.admin.port = notAPortValue
        """));

    try (
        MockedConstruction<BBjRouteSecurityManager> constructed =
            mockConstruction(BBjRouteSecurityManager.class);
        CapturedLog log = new CapturedLog(BBjRouteSecurityRegistrar.class.getPackageName())) {
      BBjSecurityError error = assertThrows(BBjSecurityError.class, () -> registrar.onWillRun(app));

      assertInstanceOf(IllegalArgumentException.class, error.getCause());
      assertTrue(error.getCause().getMessage().contains("webforj.security.bbj.admin.port"));
      assertEquals(1, log.getMessages(Level.SEVERE).size());
      assertTrue(constructed.constructed().isEmpty());
    }

    verify(renderer, never()).addObserver(any());
    mockedObjectTable.verifyNoInteractions();
  }

  @Test
  void shouldStartWithWarningWhenTuningKeyOfTheAdminConfigurationIsInvalid() {
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString("""
        webforj.security.enabled = true
        webforj.security.bbj.admin.timeout-seconds = notANumberValue
        """));

    try (CapturedLog log = new CapturedLog(BBjRouteSecurityRegistrar.class.getPackageName())) {
      assertDoesNotThrow(() -> registrar.onWillRun(app));

      assertEquals(1, log.getMessages(Level.WARNING).size());
      assertTrue(log.getMessages(Level.WARNING).get(0)
          .contains("webforj.security.bbj.admin.timeout-seconds"));
    }

    verify(renderer).addObserver(any(RouteSecurityObserver.class));
  }

  @Test
  void shouldNotReadTheAdminConfigurationWhenDisabledByConfiguration() {
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString("""
        webforj.security.enabled = false
        webforj.security.bbj.admin.port = notAPortValue
        webforj.security.bbj.admin.timeout-seconds = notANumberValue
        """));

    try (CapturedLog log = new CapturedLog(BBjRouteSecurityRegistrar.class.getPackageName())) {
      assertDoesNotThrow(() -> registrar.onWillRun(app));

      assertTrue(log.getAllMessages().stream().noneMatch(message -> message.contains("admin")));
    }

    verify(renderer, never()).addObserver(any());
  }

  private void enableSecurity() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseString("webforj.security.enabled = true"));
  }

  private CapturingHandler captureLog() {
    CapturingHandler handler = new CapturingHandler();
    Logger target = Logger.getLogger(BBjRouteSecurityRegistrar.class.getPackageName());
    target.addHandler(handler);
    handlers.add(handler);

    return handler;
  }

  private static final class CapturingHandler extends Handler {
    private final List<LogRecord> errors = new ArrayList<>();

    List<LogRecord> getErrors() {
      return errors;
    }

    @Override
    public void publish(LogRecord record) {
      if (record.getLevel().intValue() >= Level.SEVERE.intValue()) {
        errors.add(record);
      }
    }

    @Override
    public void flush() {}

    @Override
    public void close() {}
  }

  private URLClassLoader createFixtureLoader(String fixture) {
    URL folder = getClass().getResource("/fixtures/custom-evaluators/" + fixture + "/");

    return new URLClassLoader(new URL[] {folder}, getClass().getClassLoader());
  }
}
