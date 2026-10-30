package com.webforj.bbj.security;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.bbj.security.store.BBjSecurityStore;
import com.webforj.bbj.security.store.BBjThrottleStore;
import com.webforj.router.history.Location;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.params.provider.Arguments;
import org.mockito.stubbing.OngoingStubbing;
import org.mockito.verification.VerificationMode;

/** Holds a sign in service with its mocked collaborators for the tests. */
public final class SignInFixture implements AutoCloseable {
  /** The password the tests sign in with. */
  public static final String PASSWORD = "s3cret-pw";
  /** The client address the address supplier gives unless a test changes it. */
  public static final String ADDRESS = "10.0.0.7";
  /** The location opened after a sign in when nothing was requested. */
  public static final Location DEFAULT_LOCATION = new Location("/home");
  /** The location a user requested before the sign in. */
  public static final Location REQUESTED_LOCATION = new Location("/orders");
  /** The principal of the session. */
  public final BBjPrincipal principal = new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of());
  /** The session the gateway delivers. */
  public final BBjSession session =
      new BBjSession(principal, "tok-abc-123", Instant.parse("2026-01-01T13:00:00Z"));
  /** The mocked gateway. */
  public final BBjAdminGateway gateway = mock(BBjAdminGateway.class);
  /** The mocked store, which reads back the session saved last until it is cleared. */
  public final BBjSecurityStore store = mock(BBjSecurityStore.class);
  /** The mocked throttle store, which reads no failed attempts unless a test stubs it. */
  public final BBjThrottleStore throttleStore = mock(BBjThrottleStore.class);
  /** The mocked supplier of the client address, which gives {@link #ADDRESS}. */
  public final Supplier<String> addressSupplier = createAddressSupplier();
  /** The configuration properties the service reads. */
  public final BBjSecurityConfigurationProperties properties =
      new BBjSecurityConfigurationProperties(ConfigFactory.empty());
  /** The route security configuration of the properties. */
  public final BBjRouteSecurityConfiguration configuration =
      new BBjRouteSecurityConfiguration(properties);
  /** The mocked navigator. */
  public final Consumer<Location> navigator = createNavigator();
  /** The log records of the service. */
  public final List<LogRecord> records = new CopyOnWriteArrayList<>();
  private final Logger logger = Logger.getLogger(BBjAuthenticationService.class.getName());
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
  private final AtomicReference<BBjSession> storedSession = new AtomicReference<>();
  private final Level previousLevel;
  private BBjAuthenticationService service;

  /** Creates the fixture and starts capturing the log records of the service. */
  public SignInFixture() {
    previousLevel = logger.getLevel();
    logger.setLevel(Level.ALL);
    logger.addHandler(handler);
    properties.setDefaultSuccessPath("/home");
    doAnswer(call -> {
      storedSession.set(call.getArgument(0));
      return null;
    }).when(store).saveSession(any());
    doAnswer(call -> {
      storedSession.set(null);
      return null;
    }).when(store).clearSession();
    when(store.readSession()).thenAnswer(call -> Optional.ofNullable(storedSession.get()));
    service = createBuilder().build();
  }

  /** Stops capturing the log records and restores the log level. */
  @Override
  public void close() {
    logger.removeHandler(handler);
    logger.setLevel(previousLevel);
  }

  /** Gives the user names and passwords the service refuses. */
  public static Stream<Arguments> getRefusedInputs() {
    return Stream.of(Arguments.of("", PASSWORD), Arguments.of("  ", PASSWORD),
        Arguments.of("alice", ""), Arguments.of("alice", "  "),
        Arguments.of("a".repeat(65), PASSWORD), Arguments.of("alice", "p".repeat(1025)));
  }

  /** Gives the user names and passwords at the limit that the service accepts. */
  public static Stream<Arguments> getAcceptedInputs() {
    return Stream.of(Arguments.of("a".repeat(64), PASSWORD),
        Arguments.of("alice", "p".repeat(1024)));
  }

  /** Gives the configured logout paths with the location opened for each. */
  public static Stream<Arguments> getLogoutCases() {
    return Stream.of(Arguments.of("/goodbye", new Location("/goodbye")),
        Arguments.of(null, new Location("/login")));
  }

  /** Gives the return settings and requested locations with the location opened for each. */
  public static Stream<Arguments> getNavigationCases() {
    return Stream.of(Arguments.of(true, Optional.of(REQUESTED_LOCATION), REQUESTED_LOCATION),
        Arguments.of(true, Optional.empty(), DEFAULT_LOCATION),
        Arguments.of(false, Optional.of(REQUESTED_LOCATION), DEFAULT_LOCATION));
  }

  /** Creates a builder of the service with all values of the fixture. */
  public BBjAuthenticationService.Builder createBuilder() {
    return BBjAuthenticationService.builder().setGateway(gateway).setStore(store)
        .setProperties(properties).setConfiguration(configuration).setNavigator(navigator)
        .setThrottleStore(throttleStore).setAddressSupplier(addressSupplier);
  }

  /**
   * Replaces the service under test.
   *
   * @param service the service the delegates call
   */
  public void setService(BBjAuthenticationService service) {
    this.service = service;
  }

  /**
   * Signs in through the service.
   *
   * @param userName the user name
   * @param password the password
   *
   * @return the status of the sign in
   */
  public BBjLoginStatus login(String userName, String password) {
    return service.login(userName, password, null);
  }

  /**
   * Signs in through the service with a 2 factor code.
   *
   * @param userName the user name
   * @param password the password
   * @param code the 2 factor code
   *
   * @return the status of the sign in
   */
  public BBjLoginStatus login(String userName, String password, String code) {
    return service.login(userName, password, code);
  }

  /** Opens the target location through the service. */
  public void openTargetLocation() {
    service.openTargetLocation();
  }

  /** Signs out through the service. */
  public void logout() {
    service.logout();
  }

  /** Checks the sign in through the service. */
  public void check() {
    service.check();
  }

  /**
   * Gives the signed in user of the service.
   *
   * @return the principal or empty when nobody is signed in
   */
  public Optional<BBjPrincipal> currentUser() {
    return service.currentUser();
  }

  /**
   * Puts a session in the session store without a call on the mock, as another tab would.
   *
   * @param target the session the store holds, or null for an empty store
   */
  public void setStoredSession(BBjSession target) {
    storedSession.set(target);
  }

  /** Restores the session of the session store through the service. */
  public void restoreSession() {
    service.restoreSession();
  }

  /**
   * Stubs the opening of a session on the gateway.
   *
   * @param userName the user name
   * @param password the password
   *
   * @return the stubbing
   */
  public OngoingStubbing<BBjSession> whenOpenSession(String userName, String password) {
    return when(gateway.openSession(userName, password, null));
  }

  /**
   * Stubs the opening of a session with a 2 factor code on the gateway.
   *
   * @param userName the user name
   * @param password the password
   * @param code the 2 factor code
   *
   * @return the stubbing
   */
  public OngoingStubbing<BBjSession> whenOpenSession(String userName, String password,
      String code) {
    return when(gateway.openSession(userName, password, code));
  }

  /**
   * Stubs the reading of a user name on the gateway.
   *
   * @param target the session
   *
   * @return the stubbing
   */
  public OngoingStubbing<String> whenReadUserName(BBjSession target) {
    return when(gateway.readUserName(target));
  }

  /**
   * Stubs the renewal of a session on the gateway.
   *
   * @param target the session
   *
   * @return the stubbing
   */
  public OngoingStubbing<BBjSession> whenRenewSession(BBjSession target) {
    return when(gateway.renewSession(target));
  }

  /**
   * Makes the closing of a session fail on the gateway.
   *
   * @param target the session
   * @param failure the failure to throw
   */
  public void setCloseSessionFailure(BBjSession target, Throwable failure) {
    doThrow(failure).when(gateway).closeSession(target);
  }

  /**
   * Makes the reading of a user name fail on the gateway.
   *
   * @param target the session
   * @param failure the failure to throw
   */
  public void setReadUserNameFailure(BBjSession target, Throwable failure) {
    doThrow(failure).when(gateway).readUserName(target);
  }

  /**
   * Verifies the opening of a session on the gateway.
   *
   * @param mode the verification mode
   * @param userName the user name
   * @param password the password
   */
  public void verifyOpenSession(VerificationMode mode, String userName, String password) {
    verify(gateway, mode).openSession(userName, password, null);
  }

  /**
   * Verifies the closing of a session on the gateway.
   *
   * @param mode the verification mode
   * @param target the session
   */
  public void verifyCloseSession(VerificationMode mode, BBjSession target) {
    verify(gateway, mode).closeSession(target);
  }

  /**
   * Verifies the reading of a user name on the gateway.
   *
   * @param mode the verification mode
   * @param target the session
   */
  public void verifyReadUserName(VerificationMode mode, BBjSession target) {
    verify(gateway, mode).readUserName(target);
  }

  /**
   * Verifies the renewal of a session on the gateway.
   *
   * @param mode the verification mode
   * @param target the session
   */
  public void verifyRenewSession(VerificationMode mode, BBjSession target) {
    verify(gateway, mode).renewSession(target);
  }

  /**
   * Verifies the renewal of any session on the gateway.
   *
   * @param mode the verification mode
   */
  public void verifyRenewSessionOfAny(VerificationMode mode) {
    verify(gateway, mode).renewSession(any());
  }

  /** Gives the formatted warning messages of the service. */
  public List<String> getWarnings() {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == Level.WARNING)
        .map(formatter::formatMessage).toList();
  }

  /** Gives the formatted debug messages of the service. */
  public List<String> getDebugMessages() {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == Level.FINE).map(formatter::formatMessage)
        .toList();
  }

  @SuppressWarnings("unchecked")
  private static Consumer<Location> createNavigator() {
    return mock(Consumer.class);
  }

  @SuppressWarnings("unchecked")
  private static Supplier<String> createAddressSupplier() {
    Supplier<String> target = mock(Supplier.class);
    when(target.get()).thenReturn(ADDRESS);

    return target;
  }
}
