package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminFactory;
import com.basis.api.admin.BBjAdminList;
import com.basis.api.admin.BBjAdminSecurityException;
import com.basis.api.admin.BBjAdminUserPermissions;
import com.typesafe.config.ConfigFactory;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import java.io.IOException;
import java.net.InetAddress;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class BBjAdminGatewayTest {
  private static final Instant NOW = Instant.parse("2026-01-01T00:00:00Z");
  private static final String PASSWORD = "pw-secret-1";
  private static final String TOKEN = "tokenvalue1234";
  private static final String RENEWED_TOKEN = "renewedtoken5678";
  private static final String UNAVAILABLE = "BBj Admin API is unavailable: ";
  private static final String CODE = "654321";
  private static final String CODE_TOKEN = "codetoken9012";
  private static final String TRUSTSTORE = "/etc/bbj/admin.p12";
  private static final String TRUSTSTORE_PASSWORD = "store-secret-2";
  private static final String WRONG_CODE_TEXT =
      "Server failed login: Authentication failed for user "
          + "'alice'. Authentication failed. Invalid 2-factor authentication code.";
  private final Clock clock = Clock.fixed(NOW, ZoneOffset.UTC);
  private MockedStatic<BBjAdminFactory> factory;
  private BBjAdminBase admin;
  private BBjAdminUserPermissions permissions;
  private InetAddress loopback;
  private BBjAdminGateway gateway;

  @BeforeEach
  void setUp() throws Exception {
    factory = mockStatic(BBjAdminFactory.class);
    admin = mock(BBjAdminBase.class);
    permissions = mock(BBjAdminUserPermissions.class);
    when(permissions.contains(anyString())).thenReturn(true);
    when(admin.getSingleUserPermissions(TOKEN)).thenReturn(permissions);
    loopback = InetAddress.getByName("127.0.0.1");
    gateway =
        new BBjAdminGateway(new BBjSecurityConfigurationProperties(ConfigFactory.empty()), clock);
  }

  @AfterEach
  void tearDown() {
    factory.close();
  }

  private void setToken(String token) {
    factory.when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, false, 10000, "alice", PASSWORD,
        Locale.ENGLISH, null, null, 30L, Map.of())).thenReturn(token);
  }

  private void setTokenFailure(Throwable failure) {
    factory.when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, false, 10000, "alice", PASSWORD,
        Locale.ENGLISH, null, null, 30L, Map.of())).thenThrow(failure);
  }

  private void setAdmin(String token) {
    factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, token, null,
        Locale.ENGLISH, null, null)).thenReturn(admin);
  }

  private void setRenewedAdmin(BBjAdminBase renewedAdmin) throws BBjAdminException {
    when(admin.renewToken(30L, true)).thenReturn(RENEWED_TOKEN);
    when(renewedAdmin.getSingleUserPermissions(RENEWED_TOKEN)).thenReturn(permissions);
    factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, RENEWED_TOKEN,
        null, Locale.ENGLISH, null, null)).thenReturn(renewedAdmin);
  }

  private void setCodeAdmin(BBjAdminBase codeAdmin) throws BBjAdminException {
    when(codeAdmin.getTokenValue()).thenReturn(CODE_TOKEN);
    when(codeAdmin.renewToken(CODE_TOKEN, 30L)).thenReturn(TOKEN);
    factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, "alice", PASSWORD,
        Locale.ENGLISH, null, null, CODE)).thenReturn(codeAdmin);
  }

  private void setCodeAdminFailure(Throwable failure) {
    factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, "alice", PASSWORD,
        Locale.ENGLISH, null, null, CODE)).thenThrow(failure);
  }

  private BBjAdminFailure openFailingSession(Throwable failure) {
    setTokenFailure(failure);

    return assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));
  }

  private BBjAdminFailure readFailingUserName(Throwable failure) throws BBjAdminException {
    setAdmin(TOKEN);
    when(admin.getUser()).thenThrow(failure);
    BBjSession session =
        new BBjSession(new BBjPrincipal("alice", Set.of(), Set.of()), TOKEN, NOW.plusSeconds(60));

    return assertThrows(BBjAdminFailure.class, () -> gateway.readUserName(session));
  }

  private static BBjAdminList<String> createGroupNames(List<String> names) {
    return mock(delegatesTo(new ArrayList<>(names)));
  }

  private static Stream<Arguments> getGroupNames() {
    return Stream.of(Arguments.of(List.of(), Set.of()),
        Arguments.of(List.of("Admins"), Set.of("Admins")), Arguments
            .of(List.of("Admins", "admins", "Sales EU"), Set.of("Admins", "admins", "Sales EU")));
  }

  private static Stream<Arguments> getGroupNameFailures() {
    return Stream.of(
        Arguments.of(new BBjAdminException("Server failed login: Authentication token is expired"),
            Kind.TOKEN_REJECTED),
        Arguments.of(new BBjAdminException("Server did not accept the connection"),
            Kind.UNAVAILABLE),
        Arguments.of(new IllegalStateException("boom"), Kind.UNAVAILABLE));
  }

  private static Stream<Arguments> getCodeSignInFailures() {
    return Stream.of(Arguments.of(new BBjAdminException(WRONG_CODE_TEXT), Kind.INVALID_CODE),
        Arguments.of(
            new BBjAdminException("Unable to authenticate user. Invalid username/password."),
            Kind.INVALID_CREDENTIALS),
        Arguments.of(new BBjAdminException("Server did not accept the connection"),
            Kind.UNAVAILABLE),
        Arguments.of(new IllegalStateException("boom"), Kind.UNAVAILABLE));
  }

  private static Stream<Arguments> getKnownTexts() {
    return Stream.of(
        Arguments.of("Unable to authenticate user. Invalid username/password.",
            Kind.INVALID_CREDENTIALS),
        Arguments.of("Unable to authenticate user. Authentication failed. "
            + "Invalid 2-factor authentication code.", Kind.INVALID_CODE),
        Arguments.of("Unable to authenticate user. Authentication failed. "
            + "2-factor authentication code required.", Kind.TWO_FACTOR_REQUIRED),
        Arguments.of("Server failed login: Authentication token is expired", Kind.TOKEN_REJECTED),
        Arguments.of("Server failed login: Unable to verify authentication token",
            Kind.TOKEN_REJECTED),
        Arguments.of("Authentication token is expired", Kind.TOKEN_REJECTED),
        Arguments.of("Unable to verify authentication token", Kind.TOKEN_REJECTED),
        Arguments.of("Server failed login: some reason nobody knows", Kind.TOKEN_REJECTED),
        Arguments.of(WRONG_CODE_TEXT, Kind.INVALID_CODE),
        Arguments.of("Unable to communicate with server.  Server may have gone down.",
            Kind.UNAVAILABLE),
        Arguments.of("Server did not accept the connection", Kind.UNAVAILABLE),
        Arguments.of("User 'x' does not have permission to access server configurations.",
            Kind.UNAVAILABLE),
        Arguments.of("something the library has never said", Kind.UNAVAILABLE),
        Arguments.of("", Kind.UNAVAILABLE));
  }

  private static Stream<Arguments> getKindMessages() {
    return Stream.of(
        Arguments.of("Invalid username/password",
            "BBj Admin API refused the credentials: "
                + "BBjAdminException: Invalid username/password"),
        Arguments.of("2-factor authentication code required",
            "BBj Admin API requires a 2-factor "
                + "code: BBjAdminException: 2-factor authentication code required"),
        Arguments.of("Invalid 2-factor authentication code",
            "BBj Admin API refused the 2-factor code: "
                + "BBjAdminException: Invalid 2-factor authentication code"),
        Arguments.of("Server failed login: Authentication token is expired",
            "BBj Admin API refused the token: BBjAdminException: "
                + "Server failed login: Authentication token is expired"),
        Arguments.of("boom", UNAVAILABLE + "BBjAdminException: boom"));
  }

  @Nested
  class OpenSession {

    @Test
    void shouldReturnTheSessionOfTheToken() throws Exception {
      setToken(TOKEN);
      setAdmin(TOKEN);
      when(admin.getUser()).thenReturn("Alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));

      BBjSession session = gateway.openSession("alice", PASSWORD, null);

      assertEquals("Alice", session.getPrincipal().getUserName());
      assertEquals(TOKEN, session.getToken());
      assertEquals(NOW.plusSeconds(30 * 60), session.getExpiresAt());
      verify(admin, times(1)).release();
    }

    @Test
    void shouldCallTheFactoryWithTheConfiguredValues() throws Exception {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminPort(2003);
      properties.setAdminTimeoutSeconds(7);
      properties.setTokenMinutes(45);
      BBjAdminGateway configured = new BBjAdminGateway(properties, clock);
      factory.when(() -> BBjAdminFactory.getAuthToken(loopback, 2003, false, 7000, "alice",
          PASSWORD, Locale.ENGLISH, null, null, 45L, Map.of())).thenReturn(TOKEN);
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2003, false, 7000, TOKEN, null,
          Locale.ENGLISH, null, null)).thenReturn(admin);
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));

      BBjSession session = configured.openSession("alice", PASSWORD, null);

      assertEquals(NOW.plusSeconds(45 * 60), session.getExpiresAt());
      factory.verify(() -> BBjAdminFactory.getAuthToken(loopback, 2003, false, 7000, "alice",
          PASSWORD, Locale.ENGLISH, null, null, 45L, Map.of()));
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2003, false, 7000, TOKEN, null,
          Locale.ENGLISH, null, null));
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getGroupNames")
    void shouldTakeTheRolesFromTheSecurityGroupNames(List<String> groups, Set<String> roles)
        throws Exception {
      setToken(TOKEN);
      setAdmin(TOKEN);
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(groups));

      BBjPrincipal principal = gateway.openSession("alice", PASSWORD, null).getPrincipal();

      assertEquals(roles, principal.getRoles());
      assertTrue(principal.getAuthorities().isEmpty());
      verify(admin, times(1)).release();
    }

    @Test
    void shouldTakeTheAuthoritiesFromThePermissionsOfTheUser() throws Exception {
      setToken(TOKEN);
      setAdmin(TOKEN);
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));
      when(permissions.getBoolean("admin-access")).thenReturn(true);
      when(permissions.getBoolean("server-config")).thenReturn(true);

      BBjPrincipal principal = gateway.openSession("alice", PASSWORD, null).getPrincipal();

      assertEquals(Set.of("admin-access", "server-config"), principal.getAuthorities());
      verify(admin, times(1)).getSingleUserPermissions(TOKEN);
    }

    @Test
    void shouldGiveNoAuthoritiesToAUserWithoutPermissions() throws Exception {
      setToken(TOKEN);
      setAdmin(TOKEN);
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));
      when(permissions.getBoolean(anyString())).thenReturn(false);

      BBjPrincipal principal = gateway.openSession("alice", PASSWORD, null).getPrincipal();

      assertEquals(Set.of(), principal.getAuthorities());
    }

    @Test
    void shouldSendThePlainPasswordWithoutCode() throws Exception {
      setToken(TOKEN);
      setAdmin(TOKEN);
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));

      gateway.openSession("alice", PASSWORD, null);

      factory.verify(() -> BBjAdminFactory.getAuthToken(loopback, 2002, false, 10000, "alice",
          PASSWORD, Locale.ENGLISH, null, null, 30L, Map.of()));
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null));
      factory.verifyNoMoreInteractions();
    }

    @Test
    void shouldSignInWithTheCodeAndKeepTheRenewedToken() throws Exception {
      BBjAdminBase codeAdmin = mock(BBjAdminBase.class);
      setCodeAdmin(codeAdmin);
      setAdmin(TOKEN);
      when(admin.getUser()).thenReturn("Alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of("Admins")));

      BBjSession session = gateway.openSession("alice", PASSWORD, CODE);

      assertEquals(TOKEN, session.getToken());
      assertEquals(NOW.plusSeconds(30 * 60), session.getExpiresAt());
      assertEquals("Alice", session.getPrincipal().getUserName());
      assertEquals(Set.of("Admins"), session.getPrincipal().getRoles());
      verify(codeAdmin, times(1)).renewToken(CODE_TOKEN, 30L);
      verify(codeAdmin, times(1)).release();
      verify(codeAdmin, never()).getUser();
      verify(admin, times(1)).getSingleUserPermissions(TOKEN);
      verify(admin, times(1)).release();
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, "alice",
          PASSWORD, Locale.ENGLISH, null, null, CODE));
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null));
      factory.verifyNoMoreInteractions();
    }

    @Test
    void shouldSignInWithTheCodeWithTheConfiguredValues() throws Exception {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminPort(2003);
      properties.setTokenMinutes(45);
      BBjAdminGateway configured = new BBjAdminGateway(properties, clock);
      BBjAdminBase codeAdmin = mock(BBjAdminBase.class);
      when(codeAdmin.getTokenValue()).thenReturn(CODE_TOKEN);
      when(codeAdmin.renewToken(CODE_TOKEN, 45L)).thenReturn(TOKEN);
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2003, false, 10000, "alice",
          PASSWORD, Locale.ENGLISH, null, null, CODE)).thenReturn(codeAdmin);
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2003, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null)).thenReturn(admin);
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));

      BBjSession session = configured.openSession("alice", PASSWORD, CODE);

      assertEquals(TOKEN, session.getToken());
      assertEquals(NOW.plusSeconds(45 * 60), session.getExpiresAt());
      verify(codeAdmin, times(1)).release();
    }

    @Test
    void shouldSignInOverSslWithTheTruststore() throws Exception {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminSsl(true);
      properties.setAdminTruststore(TRUSTSTORE);
      properties.setAdminTruststorePassword(TRUSTSTORE_PASSWORD);
      BBjAdminGateway configured = new BBjAdminGateway(properties, clock);
      factory.when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, true, 10000, "alice",
          PASSWORD, Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD, 30L, Map.of()))
          .thenReturn(TOKEN);
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, true, 10000, TOKEN, null,
          Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD)).thenReturn(admin);
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));

      BBjSession session = configured.openSession("alice", PASSWORD, null);

      assertEquals(TOKEN, session.getToken());
      factory.verify(() -> BBjAdminFactory.getAuthToken(loopback, 2002, true, 10000, "alice",
          PASSWORD, Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD, 30L, Map.of()));
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, true, 10000, TOKEN, null,
          Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD));
      factory.verifyNoMoreInteractions();
    }

    @Test
    void shouldSignInWithTheCodeOverSslWithTheTruststoreAndTheTimeout() throws Exception {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminSsl(true);
      properties.setAdminTruststore(TRUSTSTORE);
      properties.setAdminTruststorePassword(TRUSTSTORE_PASSWORD);
      properties.setAdminTimeoutSeconds(7);
      BBjAdminGateway configured = new BBjAdminGateway(properties, clock);
      BBjAdminBase codeAdmin = mock(BBjAdminBase.class);
      when(codeAdmin.getTokenValue()).thenReturn(CODE_TOKEN);
      when(codeAdmin.renewToken(CODE_TOKEN, 30L)).thenReturn(TOKEN);
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, true, 7000, "alice", PASSWORD,
          Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD, CODE)).thenReturn(codeAdmin);
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, true, 7000, TOKEN, null,
          Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD)).thenReturn(admin);
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));

      BBjSession session = configured.openSession("alice", PASSWORD, CODE);

      assertEquals(TOKEN, session.getToken());
      verify(codeAdmin, times(1)).release();
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, true, 7000, "alice",
          PASSWORD, Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD, CODE));
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, true, 7000, TOKEN, null,
          Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD));
      factory.verifyNoMoreInteractions();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getCodeSignInFailures")
    void shouldClassifyFailureOfTheSignInWithCode(Throwable cause, Kind kind) {
      setCodeAdminFailure(cause);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, CODE));

      assertEquals(kind, failure.getKind());
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null), never());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getGroupNameFailures")
    void shouldReleaseTheCodeConnectionWhenTheTokenCannotBeRenewed(Throwable cause, Kind kind)
        throws Exception {
      BBjAdminBase codeAdmin = mock(BBjAdminBase.class);
      setCodeAdmin(codeAdmin);
      when(codeAdmin.renewToken(CODE_TOKEN, 30L)).thenThrow(cause);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, CODE));

      assertEquals(kind, failure.getKind());
      verify(codeAdmin, times(1)).release();
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null), never());
    }

    @Test
    void shouldHideThePasswordTheCodeAndTheTokenWhenTheTokenCannotBeRenewed() throws Exception {
      BBjAdminBase codeAdmin = mock(BBjAdminBase.class);
      setCodeAdmin(codeAdmin);
      when(codeAdmin.renewToken(CODE_TOKEN, 30L)).thenThrow(
          new BBjAdminException("bad " + PASSWORD + " and " + CODE + " and " + CODE_TOKEN));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, CODE));

      assertEquals(UNAVAILABLE + "BBjAdminException: bad *** and *** and ***",
          failure.getMessage());
    }

    @Test
    void shouldCreateTheGatewayWithTheSystemClock() {
      assertDoesNotThrow(
          () -> new BBjAdminGateway(new BBjSecurityConfigurationProperties(ConfigFactory.empty())));
    }

    @Test
    void shouldNotReadTheUserNameWhenTheTokenIsRefused() throws Exception {
      BBjAdminFailure failure =
          openFailingSession(new BBjAdminException("Invalid username/password"));

      assertEquals(Kind.INVALID_CREDENTIALS, failure.getKind());
      factory.verify(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null), never());
    }
  }

  @Nested
  class CloseSession {
    private final BBjSession session =
        new BBjSession(new BBjPrincipal("alice", Set.of(), Set.of()), TOKEN, NOW.plusSeconds(60));

    @BeforeEach
    void setUp() {
      setAdmin(TOKEN);
    }

    private BBjAdminFailure closeFailingSession() {
      return assertThrows(BBjAdminFailure.class, () -> gateway.closeSession(session));
    }

    @Test
    void shouldNotContactTheAdminApiForNullSession() {
      assertDoesNotThrow(() -> gateway.closeSession(null));

      factory.verifyNoInteractions();
    }

    @Test
    void shouldCloseTheTokenAndReleaseTheConnection() throws Exception {
      gateway.closeSession(session);

      verify(admin, times(1)).invalidateToken(TOKEN);
      verify(admin, times(1)).release();
    }

    @Test
    void shouldClassifyRefusedTokenAsTokenRejected() throws Exception {
      doThrow(new BBjAdminException("Server failed login: Authentication token is expired"))
          .when(admin).invalidateToken(TOKEN);

      BBjAdminFailure failure = closeFailingSession();

      assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
      verify(admin, times(1)).release();
    }

    @Test
    void shouldClassifyRuntimeFailureAsUnavailable() throws Exception {
      doThrow(new IllegalStateException("boom")).when(admin).invalidateToken(TOKEN);

      BBjAdminFailure failure = closeFailingSession();

      assertEquals(Kind.UNAVAILABLE, failure.getKind());
      assertEquals(UNAVAILABLE + "IllegalStateException: boom", failure.getMessage());
      verify(admin, times(1)).release();
    }

    @Test
    void shouldClassifyAnUnresolvableHostAsUnavailable() {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminHost("no such host.invalid");
      BBjAdminGateway unresolved = new BBjAdminGateway(properties, clock);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> unresolved.closeSession(session));

      assertEquals(Kind.UNAVAILABLE, failure.getKind());
    }

    @Test
    void shouldHideTheTokenInTheMessage() throws Exception {
      doThrow(new BBjAdminException("bad " + TOKEN + " again")).when(admin).invalidateToken(TOKEN);

      BBjAdminFailure failure = closeFailingSession();

      assertEquals(UNAVAILABLE + "BBjAdminException: bad *** again", failure.getMessage());
      assertFalse(failure.toString().contains(TOKEN));
    }
  }

  @Nested
  class RenewSession {
    private final BBjPrincipal principal =
        new BBjPrincipal("alice", Set.of("Admins", "Sales EU"), Set.of());
    private final BBjSession session = new BBjSession(principal, TOKEN, NOW.plusSeconds(60));

    private final BBjAdminBase renewedAdmin = mock(BBjAdminBase.class);

    @Test
    void shouldReadThePrincipalAndItsRolesThroughTheNewToken() throws Exception {
      setAdmin(TOKEN);
      setRenewedAdmin(renewedAdmin);
      when(renewedAdmin.getUser()).thenReturn("Alice");
      when(renewedAdmin.getSecurityGroupNames())
          .thenReturn(createGroupNames(List.of("Admins", "Support")));

      BBjSession renewed = gateway.renewSession(session);

      assertNotSame(principal, renewed.getPrincipal());
      assertEquals("Alice", renewed.getPrincipal().getUserName());
      assertEquals(Set.of("Admins", "Support"), renewed.getPrincipal().getRoles());
      assertTrue(renewed.getPrincipal().getAuthorities().isEmpty());
      assertEquals(RENEWED_TOKEN, renewed.getToken());
      assertEquals(NOW.plusSeconds(30 * 60), renewed.getExpiresAt());
      verify(admin, never()).getSecurityGroupNames();
      verify(admin, times(1)).release();
      verify(renewedAdmin, times(1)).release();
    }

    @Test
    void shouldReadTheAuthoritiesThroughTheNewToken() throws Exception {
      setAdmin(TOKEN);
      setRenewedAdmin(renewedAdmin);
      BBjAdminUserPermissions renewedPermissions = mock(BBjAdminUserPermissions.class);
      when(renewedPermissions.contains(anyString())).thenReturn(true);
      when(renewedAdmin.getUser()).thenReturn("alice");
      when(renewedAdmin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));
      when(renewedAdmin.getSingleUserPermissions(RENEWED_TOKEN)).thenReturn(renewedPermissions);
      when(renewedPermissions.getBoolean("metrics")).thenReturn(true);

      BBjSession renewed = gateway.renewSession(session);

      assertEquals(Set.of("metrics"), renewed.getPrincipal().getAuthorities());
      verify(admin, never()).getSingleUserPermissions(anyString());
    }
  }

  @Nested
  class NullArguments {

    @Test
    void shouldRefuseNullUserName() {
      String password = PASSWORD;

      assertThrows(NullPointerException.class, () -> gateway.openSession(null, password, null));
    }

    @Test
    void shouldRefuseNullPassword() {
      String userName = "alice";

      assertThrows(NullPointerException.class, () -> gateway.openSession(userName, null, null));
    }

    @Test
    void shouldRefuseNullProperties() {
      assertThrows(NullPointerException.class, () -> new BBjAdminGateway(null, clock));
    }

    @Test
    void shouldRefuseNullClock() {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());

      assertThrows(NullPointerException.class, () -> new BBjAdminGateway(properties, null));
    }
  }

  @Nested
  class KindOfFailure {

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getKnownTexts")
    void shouldClassifyTheTextOfTheException(String text, Kind kind) {
      assertEquals(kind, openFailingSession(new BBjAdminException(text)).getKind());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getKnownTexts")
    void shouldClassifyTheTextOfCause(String text, Kind kind) {
      BBjAdminException wrapped = new BBjAdminException(new IOException(text));

      assertEquals(kind, openFailingSession(wrapped).getKind());
    }

    @Test
    void shouldClassifyNullMessageAsUnavailable() {
      assertEquals(Kind.UNAVAILABLE,
          openFailingSession(new BBjAdminException((String) null)).getKind());
    }

    @Test
    void shouldClassifyTheTextWhateverItsCase() {
      BBjAdminException exception =
          new BBjAdminException("UNABLE TO AUTHENTICATE USER. INVALID USERNAME/PASSWORD.");

      assertEquals(Kind.INVALID_CREDENTIALS, openFailingSession(exception).getKind());
    }

    @Test
    void shouldPreferTheKnownTextToTheRefusedLoginPrefix() {
      BBjAdminException exception = new BBjAdminException(
          "Server failed login: Authentication failed. Invalid username/password.");

      assertEquals(Kind.INVALID_CREDENTIALS, openFailingSession(exception).getKind());
    }

    @Test
    void shouldPreferTheKnownTextToTheRefusedLoginPrefixOfCause() {
      BBjAdminException exception = new BBjAdminException("Invalid username/password",
          new IOException("Server failed login: nothing known"));

      assertEquals(Kind.INVALID_CREDENTIALS, openFailingSession(exception).getKind());
    }

    @Test
    void shouldPreferTheRefusedLoginPrefixToKnownText() throws Exception {
      BBjAdminException exception = new BBjAdminException(
          "Server failed login: Authentication failed. Invalid username/password.");

      assertEquals(Kind.TOKEN_REJECTED, readFailingUserName(exception).getKind());
    }

    @Test
    void shouldPreferTheRefusedLoginPrefixOfCause() throws Exception {
      BBjAdminException exception = new BBjAdminException("Invalid username/password",
          new IOException("Server failed login: nothing known"));

      assertEquals(Kind.TOKEN_REJECTED, readFailingUserName(exception).getKind());
    }

    @Test
    void shouldPreferTheRefusedLoginPrefixToTheCodeTextOfTokenCall() throws Exception {
      assertEquals(Kind.TOKEN_REJECTED,
          readFailingUserName(new BBjAdminException(WRONG_CODE_TEXT)).getKind());
    }

    @Test
    void shouldPreferTheCredentialsTextToTheTwoFactorText() {
      BBjAdminException exception = new BBjAdminException(
          "2-factor authentication code required. Invalid username/password.");

      assertEquals(Kind.INVALID_CREDENTIALS, openFailingSession(exception).getKind());
    }

    @Test
    void shouldClassifyPermissionRefusalAsUnavailable() {
      BBjAdminSecurityException exception =
          new BBjAdminSecurityException("User 'x' does not have permission.");

      assertEquals(Kind.UNAVAILABLE, openFailingSession(exception).getKind());
    }

    @Test
    void shouldNeverTakeVerdictFromAnExceptionThatIsNotAnAdminException() {
      IllegalStateException state = new IllegalStateException("Invalid username/password");

      assertEquals(Kind.UNAVAILABLE, openFailingSession(state).getKind());
    }

    @Test
    void shouldClassifyAnUnresolvableHostAsUnavailable() {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminHost("no such host.invalid");
      BBjAdminGateway unresolved = new BBjAdminGateway(properties, clock);

      BBjAdminFailure failure = assertThrows(BBjAdminFailure.class,
          () -> unresolved.openSession("alice", PASSWORD, null));

      assertEquals(Kind.UNAVAILABLE, failure.getKind());
    }

    @Test
    void shouldLookOnlyFiveCausesDeep() {
      Throwable chain = new BBjAdminException("Invalid username/password");
      for (int index = 0; index < 5; index++) {
        chain = new BBjAdminException("level " + index, chain);
      }

      assertEquals(Kind.UNAVAILABLE, openFailingSession(chain).getKind());
    }

    @Test
    void shouldStopAtCycleOfCauses() {
      BBjAdminException first = new BBjAdminException("first");
      BBjAdminException second = new BBjAdminException("second", first);
      first.initCause(second);

      BBjAdminFailure failure = openFailingSession(first);

      assertEquals(UNAVAILABLE + "BBjAdminException: first / BBjAdminException: second",
          failure.getMessage());
    }
  }

  @Nested
  class MessageOfFailure {

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getKindMessages")
    void shouldStartWithThePrefixOfTheKind(String text, String message) {
      assertEquals(message, openFailingSession(new BBjAdminException(text)).getMessage());
    }

    @Test
    void shouldReplaceThePassword() {
      BBjAdminException exception = new BBjAdminException("bad " + PASSWORD + " again " + PASSWORD);

      BBjAdminFailure failure = openFailingSession(exception);

      assertEquals(UNAVAILABLE + "BBjAdminException: bad *** again ***", failure.getMessage());
      assertFalse(failure.toString().contains(PASSWORD));
    }

    @Test
    void shouldReplaceThePasswordAndTheCodeOfRefusedCode() {
      setCodeAdminFailure(new BBjAdminException(WRONG_CODE_TEXT + " " + PASSWORD + " " + CODE));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, CODE));

      assertEquals(Kind.INVALID_CODE, failure.getKind());
      assertEquals("BBj Admin API refused the 2-factor code: BBjAdminException: " + WRONG_CODE_TEXT
          + " *** ***", failure.getMessage());
    }

    @Test
    void shouldReplaceTheTruststorePasswordWhenTheSignInFails() {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminSsl(true);
      properties.setAdminTruststore(TRUSTSTORE);
      properties.setAdminTruststorePassword(TRUSTSTORE_PASSWORD);
      BBjAdminGateway configured = new BBjAdminGateway(properties, clock);
      factory
          .when(() -> BBjAdminFactory.getAuthToken(loopback, 2002, true, 10000, "alice", PASSWORD,
              Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD, 30L, Map.of()))
          .thenThrow(new BBjAdminException("keystore " + TRUSTSTORE_PASSWORD + " refused"));

      BBjAdminFailure failure = assertThrows(BBjAdminFailure.class,
          () -> configured.openSession("alice", PASSWORD, null));

      assertEquals(UNAVAILABLE + "BBjAdminException: keystore *** refused", failure.getMessage());
    }

    @Test
    void shouldReplaceTheTruststorePasswordWhenTheSignInWithCodeFails() {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminSsl(true);
      properties.setAdminTruststore(TRUSTSTORE);
      properties.setAdminTruststorePassword(TRUSTSTORE_PASSWORD);
      BBjAdminGateway configured = new BBjAdminGateway(properties, clock);
      factory
          .when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, true, 10000, "alice", PASSWORD,
              Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD, CODE))
          .thenThrow(new BBjAdminException("keystore " + TRUSTSTORE_PASSWORD + " refused"));

      BBjAdminFailure failure = assertThrows(BBjAdminFailure.class,
          () -> configured.openSession("alice", PASSWORD, CODE));

      assertEquals(UNAVAILABLE + "BBjAdminException: keystore *** refused", failure.getMessage());
    }

    @Test
    void shouldReplaceTheTruststorePasswordWhenTheCallWithTheTokenFails() {
      BBjSecurityConfigurationProperties properties =
          new BBjSecurityConfigurationProperties(ConfigFactory.empty());
      properties.setAdminSsl(true);
      properties.setAdminTruststore(TRUSTSTORE);
      properties.setAdminTruststorePassword(TRUSTSTORE_PASSWORD);
      BBjAdminGateway configured = new BBjAdminGateway(properties, clock);
      factory
          .when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, true, 10000, TOKEN, null,
              Locale.ENGLISH, TRUSTSTORE, TRUSTSTORE_PASSWORD))
          .thenThrow(new BBjAdminException("keystore " + TRUSTSTORE_PASSWORD + " refused"));
      BBjSession session =
          new BBjSession(new BBjPrincipal("alice", Set.of(), Set.of()), TOKEN, NOW.plusSeconds(60));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> configured.closeSession(session));

      assertEquals(UNAVAILABLE + "BBjAdminException: keystore *** refused", failure.getMessage());
    }

    @Test
    void shouldDescribeTheCauseChainWithoutDuplicates() {
      BBjAdminException exception = new BBjAdminException("boom", new BBjAdminException("boom"));

      assertEquals(UNAVAILABLE + "BBjAdminException: boom",
          openFailingSession(exception).getMessage());
    }

    @Test
    void shouldNameTheClassAndTheMessageOfEveryCause() {
      BBjAdminException exception = new BBjAdminException("outer", new IOException("inner"));

      assertEquals(UNAVAILABLE + "BBjAdminException: outer / IOException: inner",
          openFailingSession(exception).getMessage());
    }

    @Test
    void shouldNameAnExceptionWithoutMessage() {
      BBjAdminException exception = new BBjAdminException(new IllegalStateException());

      assertEquals(UNAVAILABLE + "BBjAdminException: java.lang.IllegalStateException"
          + " / IllegalStateException", openFailingSession(exception).getMessage());
    }

    @Test
    void shouldNameAnAnonymousExceptionByItsClassName() {
      BBjAdminException anonymous = new BBjAdminException("x") {};

      assertTrue(openFailingSession(anonymous).getMessage()
          .startsWith(UNAVAILABLE + BBjAdminGatewayTest.class.getName()));
    }

    @Test
    void shouldHaveNoCauseAndNoSuppressedExceptions() {
      BBjAdminException exception = new BBjAdminException("boom");
      exception.addSuppressed(new IllegalStateException("suppressed"));

      BBjAdminFailure failure = openFailingSession(exception);

      assertNull(failure.getCause());
      assertEquals(0, failure.getSuppressed().length);
    }

    @Test
    void shouldReplaceTokenShapedText() {
      BBjAdminException exception =
          new BBjAdminException("got eyJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoiYSJ9.c2ln_-x here");

      assertEquals(UNAVAILABLE + "BBjAdminException: got *** here",
          openFailingSession(exception).getMessage());
    }

    @Test
    void shouldReplaceControlCharacters() {
      BBjAdminException exception = new BBjAdminException("one\ntwo\r\tthree\u007ffour");

      assertEquals(UNAVAILABLE + "BBjAdminException: one two  three four",
          openFailingSession(exception).getMessage());
    }

    @Test
    void shouldCutTheDetailAtThreeHundredCharacters() {
      BBjAdminException exception = new BBjAdminException("x".repeat(1000));

      assertEquals(UNAVAILABLE.length() + 300, openFailingSession(exception).getMessage().length());
    }
  }

  @Nested
  class FailureAfterToken {
    private final List<LogRecord> records = new CopyOnWriteArrayList<>();
    private final Logger logger = Logger.getLogger(BBjAdminGateway.class.getName());
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
    private final BBjSession session =
        new BBjSession(new BBjPrincipal("alice", Set.of(), Set.of()), TOKEN, NOW.plusSeconds(60));
    private final BBjAdminBase renewedAdmin = mock(BBjAdminBase.class);
    private Level previousLevel;

    @BeforeEach
    void setUp() {
      previousLevel = logger.getLevel();
      logger.setLevel(Level.ALL);
      logger.addHandler(handler);
      setToken(TOKEN);
      setAdmin(TOKEN);
    }

    @AfterEach
    void tearDown() {
      logger.removeHandler(handler);
      logger.setLevel(previousLevel);
    }

    @Test
    void shouldReleaseTheConnectionAndWarnOnceWhenTheUserNameCannotBeRead() throws Exception {
      when(admin.getUser())
          .thenThrow(new BBjAdminException("Server failed login: Authentication token is expired"));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));

      assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
      verify(admin, times(1)).release();
      List<LogRecord> warnings =
          records.stream().filter(it -> it.getLevel() == Level.WARNING).toList();
      assertEquals(1, warnings.size());
      assertTrue(warnings.getFirst().getMessage().contains("expires on its own"));
    }

    @Test
    void shouldHideTheTokenAndThePasswordInTheMessageAndTheLog() throws Exception {
      when(admin.getUser()).thenThrow(new BBjAdminException("bad " + TOKEN + " and " + PASSWORD));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));

      assertEquals(UNAVAILABLE + "BBjAdminException: bad *** and ***", failure.getMessage());
      assertTrue(records.stream().noneMatch(it -> String.valueOf(it.getMessage()).contains(TOKEN)
          || String.valueOf(it.getMessage()).contains(PASSWORD)));
      assertTrue(records.stream()
          .allMatch(it -> it.getThrown() == null
              || (!String.valueOf(it.getThrown().getMessage()).contains(TOKEN)
                  && !String.valueOf(it.getThrown().getMessage()).contains(PASSWORD))));
    }

    @Test
    void shouldClassifyRuntimeFailureOfTheUserNameAsUnavailable() throws Exception {
      when(admin.getUser()).thenThrow(new IllegalStateException("boom"));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));

      assertEquals(Kind.UNAVAILABLE, failure.getKind());
      verify(admin, times(1)).release();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getGroupNameFailures")
    void shouldClassifyFailureOfTheGroupNamesLikeFailureOfTheUserName(Throwable cause, Kind kind)
        throws Exception {
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenThrow(cause);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));

      assertEquals(kind, failure.getKind());
      verify(admin, times(1)).release();
      assertEquals(1, records.stream().filter(it -> it.getLevel() == Level.WARNING).count());
    }

    @Test
    void shouldHideTheTokenAndThePasswordWhenTheGroupNamesCannotBeRead() throws Exception {
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames())
          .thenThrow(new BBjAdminException("bad " + TOKEN + " and " + PASSWORD));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));

      assertEquals(UNAVAILABLE + "BBjAdminException: bad *** and ***", failure.getMessage());
      assertTrue(records.stream().noneMatch(it -> String.valueOf(it.getMessage()).contains(TOKEN)
          || String.valueOf(it.getMessage()).contains(PASSWORD)));
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getGroupNameFailures")
    void shouldClassifyFailureOfThePermissionsLikeFailureOfTheUserName(Throwable cause, Kind kind)
        throws Exception {
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));
      when(permissions.getBoolean(anyString())).thenThrow(cause);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));

      assertEquals(kind, failure.getKind());
      verify(admin, times(1)).release();
      assertEquals(1, records.stream().filter(it -> it.getLevel() == Level.WARNING).count());
    }

    @Test
    void shouldHideTheTokenAndThePasswordWhenThePermissionsCannotBeRead() throws Exception {
      when(admin.getUser()).thenReturn("alice");
      when(admin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));
      when(permissions.getBoolean(anyString()))
          .thenThrow(new BBjAdminException("bad " + TOKEN + " and " + PASSWORD));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));

      assertEquals(UNAVAILABLE + "BBjAdminException: bad *** and ***", failure.getMessage());
      assertTrue(records.stream().noneMatch(it -> String.valueOf(it.getMessage()).contains(TOKEN)
          || String.valueOf(it.getMessage()).contains(PASSWORD)));
    }

    @Test
    void shouldHideTheTokenWhenTheConnectionCannotBeOpened() {
      factory.when(() -> BBjAdminFactory.getBBjAdmin(loopback, 2002, false, 10000, TOKEN, null,
          Locale.ENGLISH, null, null)).thenThrow(new BBjAdminException("host " + TOKEN));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, null));

      assertEquals(Kind.UNAVAILABLE, failure.getKind());
      assertEquals(UNAVAILABLE + "BBjAdminException: host ***", failure.getMessage());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getGroupNameFailures")
    void shouldReleaseBothConnectionsAndWarnOnceWhenTheCodeTokenCannotBeRead(Throwable cause,
        Kind kind) throws Exception {
      BBjAdminBase codeAdmin = mock(BBjAdminBase.class);
      setCodeAdmin(codeAdmin);
      when(admin.getUser()).thenThrow(cause);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, CODE));

      assertEquals(kind, failure.getKind());
      verify(codeAdmin, times(1)).release();
      verify(admin, times(1)).release();
      assertEquals(1, records.stream().filter(it -> it.getLevel() == Level.WARNING).count());
    }

    @Test
    void shouldPreferTheRefusedLoginPrefixWhenTheCodeTokenCannotBeRead() throws Exception {
      BBjAdminBase codeAdmin = mock(BBjAdminBase.class);
      setCodeAdmin(codeAdmin);
      when(admin.getUser()).thenThrow(new BBjAdminException(
          "Server failed login: Authentication failed. Invalid username/password."));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, CODE));

      assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
    }

    @Test
    void shouldHideThePasswordTheCodeAndTheTokenWhenTheCodeTokenCannotBeRead() throws Exception {
      BBjAdminBase codeAdmin = mock(BBjAdminBase.class);
      setCodeAdmin(codeAdmin);
      when(admin.getUser())
          .thenThrow(new BBjAdminException("bad " + TOKEN + " and " + PASSWORD + " and " + CODE));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.openSession("alice", PASSWORD, CODE));

      assertEquals(UNAVAILABLE + "BBjAdminException: bad *** and *** and ***",
          failure.getMessage());
      assertTrue(records.stream()
          .noneMatch(it -> String.valueOf(it.getMessage()).contains(TOKEN)
              || String.valueOf(it.getMessage()).contains(PASSWORD)
              || String.valueOf(it.getMessage()).contains(CODE)));
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getGroupNameFailures")
    void shouldClassifyFailureOfTheRenewedUserNameLikeOtherFailures(Throwable cause, Kind kind)
        throws Exception {
      setRenewedAdmin(renewedAdmin);
      when(renewedAdmin.getUser()).thenThrow(cause);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.renewSession(session));

      assertEquals(kind, failure.getKind());
      verify(admin, times(1)).release();
      verify(renewedAdmin, times(1)).release();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getGroupNameFailures")
    void shouldClassifyFailureOfTheRenewedGroupNamesLikeOtherFailures(Throwable cause, Kind kind)
        throws Exception {
      setRenewedAdmin(renewedAdmin);
      when(renewedAdmin.getUser()).thenReturn("alice");
      when(renewedAdmin.getSecurityGroupNames()).thenThrow(cause);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.renewSession(session));

      assertEquals(kind, failure.getKind());
      verify(admin, times(1)).release();
      verify(renewedAdmin, times(1)).release();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAdminGatewayTest#getGroupNameFailures")
    void shouldClassifyFailureOfTheRenewedPermissionsLikeOtherFailures(Throwable cause, Kind kind)
        throws Exception {
      setRenewedAdmin(renewedAdmin);
      when(renewedAdmin.getUser()).thenReturn("alice");
      when(renewedAdmin.getSecurityGroupNames()).thenReturn(createGroupNames(List.of()));
      when(permissions.getBoolean(anyString())).thenThrow(cause);

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.renewSession(session));

      assertEquals(kind, failure.getKind());
      verify(admin, times(1)).release();
      verify(renewedAdmin, times(1)).release();
    }

    @Test
    void shouldHideTheOldAndTheNewTokenWhenTheRenewedGroupNamesCannotBeRead() throws Exception {
      setRenewedAdmin(renewedAdmin);
      when(renewedAdmin.getUser()).thenReturn("alice");
      when(renewedAdmin.getSecurityGroupNames())
          .thenThrow(new BBjAdminException("bad " + TOKEN + " and " + RENEWED_TOKEN));

      BBjAdminFailure failure =
          assertThrows(BBjAdminFailure.class, () -> gateway.renewSession(session));

      assertEquals(UNAVAILABLE + "BBjAdminException: bad *** and ***", failure.getMessage());
      assertTrue(records.stream().noneMatch(it -> String.valueOf(it.getMessage()).contains(TOKEN)
          || String.valueOf(it.getMessage()).contains(RENEWED_TOKEN)));
    }
  }
}
