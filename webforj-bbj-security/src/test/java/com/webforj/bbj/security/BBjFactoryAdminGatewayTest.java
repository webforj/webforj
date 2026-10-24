package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminFactory;
import com.basis.api.admin.BBjAdminSecurityException;
import com.typesafe.config.ConfigFactory;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.fixture.CapturedLog;
import java.io.IOException;
import java.net.InetAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Locale;
import java.util.Map;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class BBjFactoryAdminGatewayTest {
  private static final Instant NOW = Instant.parse("2026-01-01T00:00:00Z");
  private static final String PASSWORD = "pw-secret-1";
  private static final String TOKEN = "tokenvalue1234";
  private static final String NEW_TOKEN = "newtokenvalue5678";
  private static final String EXPIRED = "Server failed login: Authentication token is expired";
  private static final String WRONG_PASSWORD =
      "Unable to authenticate user. Invalid username/password.";
  private final Clock clock = Clock.fixed(NOW, ZoneOffset.UTC);
  private MockedStatic<BBjAdminFactory> factory;
  private BBjAdminBase admin;
  private InetAddress loopback;
  private BBjFactoryAdminGateway gateway;

  @BeforeEach
  void setUp() throws Exception {
    factory = mockStatic(BBjAdminFactory.class);
    admin = mock(BBjAdminBase.class);
    loopback = InetAddress.getByName("127.0.0.1");
    gateway = new BBjFactoryAdminGateway(createConfiguration(""), clock);
  }

  @AfterEach
  void tearDown() {
    factory.close();
  }

  private static BBjAdminConfiguration createConfiguration(String hocon) {
    return BBjAdminConfiguration.create(ConfigFactory.parseString(hocon));
  }

  private BBjLoginSession createSession() {
    return new BBjLoginSession("alice", TOKEN, NOW.minusSeconds(60));
  }

  private void stubLogin(String token) {
    factory.when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, false, 10000, "alice", PASSWORD,
        Locale.ENGLISH, null, null, 30L, Map.of())).thenReturn(token);
  }

  private void stubConnection(String token) {
    factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, token, null,
        Locale.ENGLISH, null, null)).thenReturn(admin);
  }

  private static Stream<Arguments> getOperations() {
    BBjLoginSession session = new BBjLoginSession("alice", TOKEN, NOW);

    return Stream.of(
        Arguments.of(Named.of("openSession",
            (Consumer<BBjFactoryAdminGateway>) target -> target.openSession("alice", PASSWORD))),
        Arguments.of(Named.of("readUserName",
            (Consumer<BBjFactoryAdminGateway>) target -> target.readUserName(session))),
        Arguments.of(Named.of("updateSession",
            (Consumer<BBjFactoryAdminGateway>) target -> target.updateSession(session))),
        Arguments.of(Named.of("closeSession",
            (Consumer<BBjFactoryAdminGateway>) target -> target.closeSession(session))));
  }

  private static Stream<Arguments> getFailures() {
    return getOperations().flatMap(operation -> Stream.of(
        Arguments.of(operation.get()[0], new BBjAdminException(EXPIRED), Kind.TOKEN_REJECTED),
        Arguments.of(operation.get()[0],
            new BBjAdminSecurityException("User 'x' does not have permission."), Kind.UNAVAILABLE),
        Arguments.of(operation.get()[0], new IllegalStateException("boom"), Kind.UNAVAILABLE)));
  }

  @Nested
  class OpenSession {

    @Test
    void shouldLogInWithTheConfiguredValuesAndReadTheUserName() throws Exception {
      stubLogin(TOKEN);
      stubConnection(TOKEN);
      when(admin.getUser()).thenReturn("alice");

      BBjLoginSession session = gateway.openSession("alice", PASSWORD);

      assertEquals("alice", session.getUserName());
      assertEquals(TOKEN, session.getToken());
      assertEquals(NOW.plusSeconds(30 * 60), session.getExpiresAt());
      verify(admin, times(1)).release();
      verify(admin, never()).renewToken(1L, true);
      verify(admin, never()).invalidateToken(anyString());
    }

    @Test
    void shouldTakeTheUserNameFromTheServer() throws Exception {
      stubLogin(TOKEN);
      stubConnection(TOKEN);
      when(admin.getUser()).thenReturn("Alice");

      assertEquals("Alice", gateway.openSession("alice", PASSWORD).getUserName());
    }

    @Test
    void shouldNotStripATwoFactorSuffixOfThePassword() throws Exception {
      factory
          .when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, false, 10000, "alice",
              PASSWORD + "||TOTP=123456", Locale.ENGLISH, null, null, 30L, Map.of()))
          .thenReturn(TOKEN);
      stubConnection(TOKEN);
      when(admin.getUser()).thenReturn("alice");

      assertEquals(TOKEN, gateway.openSession("alice", PASSWORD + "||TOTP=123456").getToken());
    }

    @Test
    void shouldEndTheNewTokenWhenTheUserNameCannotBeRead() throws Exception {
      stubLogin(TOKEN);
      stubConnection(TOKEN);
      when(admin.getUser()).thenThrow(new BBjAdminException(EXPIRED));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD));

      assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
      verify(admin).renewToken(1L, true);
      verify(admin, times(2)).release();
    }

    @Test
    void shouldThrowTheOriginalFailureAndLogOneWarningWhenTheCleanupFails() throws Exception {
      stubLogin(TOKEN);
      factory
          .when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
              Locale.ENGLISH, null, null))
          .thenReturn(admin)
          .thenThrow(new BBjAdminException("cleanup said " + TOKEN + " and " + PASSWORD));
      when(admin.getUser()).thenThrow(new BBjAdminException(EXPIRED));

      try (CapturedLog log = new CapturedLog(BBjFactoryAdminGateway.class.getPackageName())) {
        BBjAdminFailure failure =
            assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD));

        assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
        assertEquals(1, log.getMessages(Level.WARNING).size());
        assertTrue(log.getAllMessages().stream()
            .noneMatch(message -> message.contains(TOKEN) || message.contains(PASSWORD)));
      }
    }

    @Test
    void shouldClassifyAWrongPasswordAndNeverOpenAConnection() {
      factory
          .when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, false, 10000, "alice", PASSWORD,
              Locale.ENGLISH, null, null, 30L, Map.of()))
          .thenThrow(new BBjAdminException(WRONG_PASSWORD + " " + PASSWORD));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD));

      assertEquals(Kind.INVALID_CREDENTIALS, failure.getKind());
      assertFalse(failure.getMessage().contains(PASSWORD));
      assertNull(failure.getCause());
      assertEquals(0, failure.getSuppressed().length);
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null), never());
    }
  }

  @Nested
  class ReadUserName {

    @Test
    void shouldReturnTheUserNameOfTheToken() throws Exception {
      stubConnection(TOKEN);
      when(admin.getUser()).thenReturn("alice");

      assertEquals("alice", gateway.readUserName(createSession()));
      verify(admin, times(1)).release();
    }

    @Test
    void shouldClassifyARejectedTokenWithoutAConnectionToRelease() {
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null)).thenThrow(new BBjAdminException(EXPIRED + " " + TOKEN));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.readUserName(createSession()));

      assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
      assertFalse(failure.getMessage().contains(TOKEN));
      verify(admin, never()).release();
    }
  }

  @Nested
  class UpdateSession {

    @Test
    void shouldRenewTheTokenAndKeepTheUserName() throws Exception {
      stubConnection(TOKEN);
      when(admin.renewToken(30L, true)).thenReturn(NEW_TOKEN);
      BBjLoginSession session = createSession();

      BBjLoginSession updated = gateway.updateSession(session);

      assertEquals("alice", updated.getUserName());
      assertEquals(NEW_TOKEN, updated.getToken());
      assertEquals(NOW.plusSeconds(30 * 60), updated.getExpiresAt());
      assertEquals(TOKEN, session.getToken());
      verify(admin, times(1)).release();
    }

    @Test
    void shouldReleaseTheConnectionWhenTheRenewalFails() throws Exception {
      stubConnection(TOKEN);
      when(admin.renewToken(30L, true)).thenThrow(new BBjAdminException(EXPIRED));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.updateSession(createSession()));

      assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
      verify(admin, times(1)).release();
    }

    @Test
    void shouldHideTheNewTokenWhenTheSessionCannotBeCreated() throws Exception {
      stubConnection(TOKEN);
      when(admin.renewToken(30L, true)).thenReturn("  ");

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.updateSession(createSession()));

      assertEquals(Kind.UNAVAILABLE, failure.getKind());
    }
  }

  @Nested
  class CloseSession {

    @Test
    void shouldRenewForOneMinuteAndDropTheReplacementToken() throws Exception {
      stubConnection(TOKEN);
      when(admin.renewToken(1L, true)).thenReturn(NEW_TOKEN);

      try (CapturedLog log = new CapturedLog(BBjFactoryAdminGateway.class.getPackageName())) {
        gateway.closeSession(createSession());

        assertTrue(log.getAllMessages().stream().noneMatch(message -> message.contains(NEW_TOKEN)));
      }

      verify(admin).renewToken(1L, true);
      verify(admin, times(1)).release();
      verify(admin, never()).invalidateToken(anyString());
    }
  }

  @Nested
  class Connection {
    @TempDir
    Path directory;

    private BBjFactoryAdminGateway createSecureGateway(String host) throws IOException {
      Path truststore = Files.writeString(directory.resolve("trust.jks"), "store");

      return new BBjFactoryAdminGateway(createConfiguration("""
          webforj.security.bbj.admin.host = %s
          webforj.security.bbj.admin.port = 2003
          webforj.security.bbj.admin.ssl = true
          webforj.security.bbj.admin.truststore = "%s"
          webforj.security.bbj.admin.truststore-password = trustpw
          webforj.security.bbj.admin.timeout-seconds = 7
          webforj.security.bbj.token.minutes = 45
          """.formatted(host, truststore)), clock);
    }

    @Test
    void shouldPassTheTruststoreTheTimeoutAndTheLifetimeToEveryCall() throws Exception {
      BBjFactoryAdminGateway secure = createSecureGateway("localhost");
      String truststore = directory.resolve("trust.jks").toString();
      InetAddress address = InetAddress.getByName("localhost");
      factory.when(() -> BBjAdminFactory.getAuthToken(address, 2003, true, 7000, "alice", PASSWORD,
          Locale.ENGLISH, truststore, "trustpw", 45L, Map.of())).thenReturn(TOKEN);
      factory.when(() -> BBjAdminFactory.getBBjAdmin(address, 2003, true, 7000, TOKEN, null,
          Locale.ENGLISH, truststore, "trustpw")).thenReturn(admin);
      when(admin.getUser()).thenReturn("alice");
      when(admin.renewToken(45L, true)).thenReturn(NEW_TOKEN);

      BBjLoginSession session = secure.openSession("alice", PASSWORD);
      BBjLoginSession updated = secure.updateSession(session);

      assertEquals(NOW.plusSeconds(45 * 60), session.getExpiresAt());
      assertEquals(NEW_TOKEN, updated.getToken());
      assertEquals("alice", secure.readUserName(session));
      secure.closeSession(session);
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(address, 2003, true, 7000, TOKEN, null,
          Locale.ENGLISH, truststore, "trustpw"), times(4));
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjFactoryAdminGatewayTest#getOperations")
    void shouldClassifyAnUnknownHostAsUnavailable(Consumer<BBjFactoryAdminGateway> operation)
        throws IOException {
      BBjFactoryAdminGateway unknown = createSecureGateway("unknown.invalid");

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> operation.accept(unknown));

      assertEquals(Kind.UNAVAILABLE, failure.getKind());
    }
  }

  @Nested
  class Failures {

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjFactoryAdminGatewayTest#getFailures")
    void shouldTurnEveryFailureOfEveryCallIntoAFailureWithAKind(
        Consumer<BBjFactoryAdminGateway> operation, Exception thrown, Kind kind) {
      factory.when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, false, 10000, "alice",
          PASSWORD, Locale.ENGLISH, null, null, 30L, Map.of())).thenAnswer(call -> {
            throw thrown;
          });
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null)).thenAnswer(call -> {
            throw thrown;
          });

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> operation.accept(gateway));

      assertEquals(kind, failure.getKind());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjFactoryAdminGatewayTest#getOperations")
    void shouldLetAnErrorPass(Consumer<BBjFactoryAdminGateway> operation) {
      factory.when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, false, 10000, "alice",
          PASSWORD, Locale.ENGLISH, null, null, 30L, Map.of())).thenThrow(new LinkageError("x"));
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null)).thenThrow(new LinkageError("x"));

      assertThrows(LinkageError.class, () -> operation.accept(gateway));
    }
  }

  @Nested
  class NullArguments {

    @Test
    void shouldRefuseNullArgumentsWithoutCallingTheFactory() {
      assertThrows(NullPointerException.class, () -> gateway.openSession(null, PASSWORD));
      assertThrows(NullPointerException.class, () -> gateway.openSession("alice", null));
      assertThrows(NullPointerException.class, () -> gateway.readUserName(null));
      assertThrows(NullPointerException.class, () -> gateway.updateSession(null));
      assertThrows(NullPointerException.class, () -> gateway.closeSession(null));
      assertThrows(NullPointerException.class, () -> new BBjFactoryAdminGateway(null, clock));
      assertThrows(NullPointerException.class,
          () -> new BBjFactoryAdminGateway(createConfiguration(""), null));
      factory.verifyNoInteractions();
    }
  }
}
