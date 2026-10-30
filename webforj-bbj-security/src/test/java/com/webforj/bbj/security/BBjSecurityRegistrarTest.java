package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.environment.ObjectTable;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.security.RouteSecurityObserver;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

class BBjSecurityRegistrarTest {
  private static final String TRUSTSTORE_PASSWORD = "truststore-pass";
  private static final String ADMIN_KEYS = """
      webforj.security.bbj.admin {
        host = "%s"
        ssl = %s
        truststore = "%s"
        truststore-password = "%s"
      }
      """;
  private final Logger logger = Logger.getLogger("com.webforj.bbj.security");
  private final List<LogRecord> records = new ArrayList<>();
  private final Handler handler = new Handler() {
    @Override
    public void publish(LogRecord logRecord) {
      records.add(logRecord);
    }

    @Override
    public void flush() {
      // Not implemented
    }

    @Override
    public void close() {
      // Not implemented
    }
  };
  private Level previousLevel;
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
    previousLevel = logger.getLevel();
    logger.setLevel(Level.ALL);
    logger.addHandler(handler);
  }

  @AfterEach
  void tearDown() {
    logger.removeHandler(handler);
    logger.setLevel(previousLevel);
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
      tableMock.when(() -> ObjectTable.contains(BBjSecurity.SERVICE_KEY)).thenReturn(true);
      tableMock.when(() -> ObjectTable.get(BBjSecurity.SERVICE_KEY))
          .thenAnswer(invocation -> services.constructed().get(0));

      assertEquals(1, contexts.size());
      assertTrue(contexts.get(0).isAuthenticated());
      assertEquals(Optional.of(principal), contexts.get(0).getPrincipal());
      BBjAuthenticationService registered = services.constructed().get(0);
      tableMock.verify(() -> ObjectTable.put(BBjSecurity.SERVICE_KEY, registered));
    }
  }

  @Test
  void shouldShareOneStoreBetweenTheServiceAndTheSecurityManager() {
    List<Object> serviceStores = new ArrayList<>();
    List<Object> managerStores = new ArrayList<>();
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());

    try (MockedConstruction<BBjAuthenticationService.Builder> builders =
        mockConstruction(BBjAuthenticationService.Builder.class,
            withSettings().defaultAnswer(RETURNS_SELF), (builder, context) -> {
              when(builder.setStore(any())).thenAnswer(invocation -> {
                serviceStores.add(invocation.getArgument(0));

                return builder;
              });
              var mockService = mock(BBjAuthenticationService.class);
              when(builder.build()).thenReturn(mockService);
            });
        MockedConstruction<BBjRouteSecurityManager> managers =
            mockConstruction(BBjRouteSecurityManager.class,
                (manager, context) -> managerStores.add(context.arguments().get(2)))) {
      registrar.onWillRun(app);

      assertEquals(1, serviceStores.size());
      assertEquals(1, managerStores.size());
      assertSame(serviceStores.get(0), managerStores.get(0));
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  void shouldGiveTheServiceTheLogoutLocationAndTheRouterNavigation() {
    List<Object> configurations = new ArrayList<>();
    List<Object> navigators = new ArrayList<>();
    Location logoutLocation = new Location("/goodbye");
    when(environment.getConfig()).thenReturn(
        ConfigFactory.parseString("webforj.security.bbj.logout-success-path = \"/goodbye\""));

    try (MockedConstruction<BBjAuthenticationService.Builder> builders =
        mockConstruction(BBjAuthenticationService.Builder.class,
            withSettings().defaultAnswer(RETURNS_SELF), (builder, context) -> {
              when(builder.setConfiguration(any())).thenAnswer(invocation -> {
                configurations.add(invocation.getArgument(0));

                return builder;
              });
              when(builder.setNavigator(any())).thenAnswer(invocation -> {
                navigators.add(invocation.getArgument(0));

                return builder;
              });
              var mockService = mock(BBjAuthenticationService.class);
              when(builder.build()).thenReturn(mockService);
            })) {
      registrar.onWillRun(app);

      BBjRouteSecurityConfiguration configuration =
          (BBjRouteSecurityConfiguration) configurations.get(0);
      Consumer<Location> navigator = (Consumer<Location>) navigators.get(0);
      navigator.accept(logoutLocation);

      assertEquals(Optional.of(logoutLocation), configuration.getLogoutLocation());
      verify(router).navigate(logoutLocation);
    }
  }

  @Test
  void shouldRestoreTheSessionBetweenRegisteringTheServiceAndAddingTheObserver() {
    List<String> events = new ArrayList<>();
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());
    tableMock.when(() -> ObjectTable.put(eq(BBjSecurity.SERVICE_KEY), any()))
        .thenAnswer(invocation -> events.add("put"));
    doAnswer(invocation -> events.add(invocation.getArgument(0).getClass().getSimpleName()))
        .when(renderer).addObserver(any());

    try (MockedConstruction<BBjAuthenticationService> services =
        mockConstruction(BBjAuthenticationService.class,
            (service, context) -> doAnswer(invocation -> events.add("restore")).when(service)
                .restoreSession())) {
      registrar.onWillRun(app);

      assertEquals(List.of("BBjSessionObserver", "put", "restore", "RouteSecurityObserver"),
          events);
    }
  }

  @ParameterizedTest
  @ValueSource(strings = {"localhost", "LOCALHOST", "127.0.0.1", "127.10.20.30", "127.255.255.254",
      "::1", "0:0:0:0:0:0:0:1"})
  void shouldRegisterTheServiceForLoopbackHostWithoutSsl(String host) {
    when(environment.getConfig()).thenReturn(
        ConfigFactory.parseString("webforj.security.bbj.admin.host = \"%s\"".formatted(host)));

    registrar.onWillRun(app);

    tableMock.verify(
        () -> ObjectTable.put(eq(BBjSecurity.SERVICE_KEY), any(BBjAuthenticationService.class)));
    assertTrue(getRecords(Level.SEVERE).isEmpty());
  }

  @Test
  void shouldRegisterTheServiceForLoopbackHostWithSslAndWithoutTruststore() {
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString("""
        webforj.security.bbj.admin {
          host = "localhost"
          ssl = true
        }
        """));

    registrar.onWillRun(app);

    tableMock.verify(
        () -> ObjectTable.put(eq(BBjSecurity.SERVICE_KEY), any(BBjAuthenticationService.class)));
    assertTrue(getRecords(Level.SEVERE).isEmpty());
  }

  @ParameterizedTest
  @ValueSource(strings = {"10.0.0.7", "128.0.0.1", "127.1", "::2", "bbj.example.com"})
  void shouldRegisterNoServiceForRemoteHostWithoutSsl(String host) {
    when(environment.getConfig()).thenReturn(
        ConfigFactory.parseString("webforj.security.bbj.admin.host = \"%s\"".formatted(host)));

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
    verify(renderer).addObserver(any(RouteSecurityObserver.class));
    assertEquals(
        List.of("Sign in is unavailable because webforj.security.bbj.admin.host is not a "
            + "loopback address, so webforj.security.bbj.admin.ssl must be true"),
        getRecords(Level.SEVERE));
  }

  @Test
  void shouldRegisterNoServiceForRemoteHostWithoutTruststore() {
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString("""
        webforj.security.bbj.admin {
          host = "bbj.example.com"
          ssl = true
        }
        """));

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
    verify(renderer).addObserver(any(RouteSecurityObserver.class));
    assertEquals(
        List.of("Sign in is unavailable because webforj.security.bbj.admin.host is not a "
            + "loopback address, so webforj.security.bbj.admin.truststore must be set"),
        getRecords(Level.SEVERE));
  }

  @Test
  void shouldRegisterTheServiceForRemoteHostWithSslAndLoadableTruststore() throws Exception {
    String truststore = getFixturePath("truststore.p12");
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString(
        ADMIN_KEYS.formatted("bbj.example.com", "true", truststore, TRUSTSTORE_PASSWORD)));

    registrar.onWillRun(app);

    tableMock.verify(
        () -> ObjectTable.put(eq(BBjSecurity.SERVICE_KEY), any(BBjAuthenticationService.class)));
    assertTrue(getRecords(Level.SEVERE).isEmpty());
    assertTrue(getRecords(Level.INFO)
        .contains("BBj Admin API at bbj.example.com:2002 with ssl true and truststore set"));
  }

  @ParameterizedTest
  @MethodSource("unreadableTruststores")
  void shouldRegisterNoServiceWhenTheTruststoreCannotBeRead(String host, String fixture,
      String password) throws Exception {
    String truststore = getFixturePath(fixture);
    when(environment.getConfig()).thenReturn(
        ConfigFactory.parseString(ADMIN_KEYS.formatted(host, "true", truststore, password)));

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
    verify(renderer).addObserver(any(RouteSecurityObserver.class));
    assertEquals(
        List.of("Sign in is unavailable because webforj.security.bbj.admin.truststore "
            + "cannot be read with webforj.security.bbj.admin.truststore-password"),
        getRecords(Level.SEVERE));
  }

  @ParameterizedTest
  @MethodSource("missingTruststores")
  void shouldRegisterNoServiceWhenTheTruststoreIsNoReadableFile(String host, String fixture)
      throws Exception {
    String truststore = getFixturePath("truststore.p12").replace("truststore.p12", fixture);
    when(environment.getConfig()).thenReturn(ConfigFactory
        .parseString(ADMIN_KEYS.formatted(host, "true", truststore, TRUSTSTORE_PASSWORD)));

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
    verify(renderer).addObserver(any(RouteSecurityObserver.class));
    assertEquals(List.of("Sign in is unavailable because webforj.security.bbj.admin.truststore "
        + "must name a readable file"), getRecords(Level.SEVERE));
  }

  @ParameterizedTest
  @MethodSource("emptyTruststores")
  void shouldRegisterNoServiceWhenTheTruststoreHoldsNoCertificates(String fixture,
      String passwordKey) throws Exception {
    String truststore = getFixturePath(fixture);
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString("""
        webforj.security.bbj.admin {
          host = "bbj.example.com"
          ssl = true
          truststore = "%s"
          %s
        }
        """.formatted(truststore, passwordKey)));

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
    verify(renderer).addObserver(any(RouteSecurityObserver.class));
    assertEquals(List.of("Sign in is unavailable because webforj.security.bbj.admin.truststore "
        + "holds no certificates"), getRecords(Level.SEVERE));
  }

  @ParameterizedTest
  @ValueSource(strings = {"a = ${b}\nb = ${a}", "a = 1 ${b}\nb = {x = 1}",
      "webforj.security.bbj.admin.host = ${BBJ_SECURITY_TEST_UNSET_HOST}"})
  void shouldRegisterNoServiceWhenTheConfigurationCannotBeResolved(String hocon) {
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString(hocon));

    registrar.onWillRun(app);

    tableMock.verify(() -> ObjectTable.put(any(String.class), any()), never());
    verify(renderer).addObserver(any(RouteSecurityObserver.class));
    assertEquals(List.of("Sign in is unavailable because config could not be resolved"),
        getRecords(Level.SEVERE));
  }

  @Test
  void shouldLogNeitherTheTruststoreNorItsPasswordWhenTheTruststoreCannotBeRead() throws Exception {
    String truststore = getFixturePath("truststore.p12");
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString(
        ADMIN_KEYS.formatted("bbj.example.com", "true", truststore, "wrong-store-secret")));

    registrar.onWillRun(app);

    List<String> messages = records.stream().map(new SimpleFormatter()::formatMessage).toList();
    assertFalse(messages.isEmpty());
    assertTrue(messages.stream().noneMatch(
        message -> message.contains("wrong-store-secret") || message.contains(truststore)));
  }

  @Test
  void shouldWarnThatTheTruststoreIsIgnoredWithoutSsl() throws Exception {
    String truststore = getFixturePath("truststore.p12");
    when(environment.getConfig()).thenReturn(ConfigFactory
        .parseString(ADMIN_KEYS.formatted("localhost", "false", truststore, TRUSTSTORE_PASSWORD)));

    registrar.onWillRun(app);

    tableMock.verify(
        () -> ObjectTable.put(eq(BBjSecurity.SERVICE_KEY), any(BBjAuthenticationService.class)));
    assertTrue(getRecords(Level.WARNING)
        .contains("The configuration key webforj.security.bbj.admin.truststore is ignored while "
            + "webforj.security.bbj.admin.ssl is false"));
  }

  private List<String> getRecords(Level level) {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == level).map(formatter::formatMessage)
        .toList();
  }

  private static String getFixturePath(String name) throws URISyntaxException {
    return Path.of(
        BBjSecurityRegistrarTest.class.getResource("/fixtures/admin-truststore/" + name).toURI())
        .toString();
  }

  private static Stream<Arguments> unreadableTruststores() {
    return Stream.of(Arguments.of("bbj.example.com", "truststore.p12", "wrong-store-secret"),
        Arguments.of("localhost", "truststore.p12", "wrong-store-secret"),
        Arguments.of("bbj.example.com", "not-a-truststore.txt", TRUSTSTORE_PASSWORD));
  }

  private static Stream<Arguments> emptyTruststores() {
    return Stream.of(Arguments.of("truststore.p12", ""), Arguments.of("empty-truststore.p12",
        "truststore-password = \"%s\"".formatted(TRUSTSTORE_PASSWORD)));
  }

  private static Stream<Arguments> missingTruststores() {
    return Stream.of(Arguments.of("bbj.example.com", "absent.p12"),
        Arguments.of("localhost", "absent.p12"), Arguments.of("bbj.example.com", ""));
  }
}
