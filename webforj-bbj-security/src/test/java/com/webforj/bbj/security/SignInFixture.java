package com.webforj.bbj.security;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.router.history.Location;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.params.provider.Arguments;
import org.mockito.ArgumentCaptor;

final class SignInFixture implements AutoCloseable {
  static final String PASSWORD = "s3cret-pw";
  static final Location DEFAULT_LOCATION = new Location("/home");
  static final Location REQUESTED_LOCATION = new Location("/orders");
  final BBjPrincipal principal = new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of());
  final BBjSession session =
      new BBjSession(principal, "tok-abc-123", Instant.parse("2026-01-01T13:00:00Z"));
  final BBjAdminGateway gateway = mock(BBjAdminGateway.class);
  final BBjAdminWorker worker = mock(BBjAdminWorker.class);
  final BBjRequestedLocationStore locationStore = mock(BBjRequestedLocationStore.class);
  final BBjSessionStore sessionStore = mock(BBjSessionStore.class);
  final BBjSecurityConfigurationProperties properties = new BBjSecurityConfigurationProperties();
  final BBjRouteSecurityConfiguration configuration = new BBjRouteSecurityConfiguration(properties);
  final Consumer<Location> navigator = createNavigator();
  final PendingResult<BBjLoginStatus> pending = new PendingResult<>();
  final ArgumentCaptor<Supplier<BBjSession>> workCaptor = createCaptor(Supplier.class);
  final ArgumentCaptor<Function<BBjSession, BBjLoginStatus>> deliverCaptor =
      createCaptor(Function.class);
  final ArgumentCaptor<Function<BBjAdminFailure, BBjLoginStatus>> failCaptor =
      createCaptor(Function.class);
  final ArgumentCaptor<Consumer<BBjSession>> discardCaptor = createCaptor(Consumer.class);
  final List<LogRecord> records = new CopyOnWriteArrayList<>();
  BBjAuthenticationService service;
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
  private final Level previousLevel;

  SignInFixture() {
    previousLevel = logger.getLevel();
    logger.setLevel(Level.ALL);
    logger.addHandler(handler);
    properties.setDefaultSuccessPath("/home");
    doReturn(pending).when(worker).start(any(), any(), any(), any());
    service = createBuilder().build();
  }

  @Override
  public void close() {
    logger.removeHandler(handler);
    logger.setLevel(previousLevel);
  }

  static Stream<Arguments> getRefusedInputs() {
    return Stream.of(Arguments.of("", PASSWORD), Arguments.of("  ", PASSWORD),
        Arguments.of("alice", ""), Arguments.of("alice", "  "),
        Arguments.of("a".repeat(257), PASSWORD), Arguments.of("alice", "p".repeat(1025)));
  }

  static Stream<Arguments> getAcceptedInputs() {
    return Stream.of(Arguments.of("a".repeat(256), PASSWORD),
        Arguments.of("alice", "p".repeat(1024)));
  }

  static Stream<Arguments> getLogoutCases() {
    return Stream.of(Arguments.of("/goodbye", new Location("/goodbye")),
        Arguments.of(null, new Location("/login")));
  }

  static Stream<Arguments> getNavigationCases() {
    return Stream.of(Arguments.of(true, Optional.of(REQUESTED_LOCATION), REQUESTED_LOCATION),
        Arguments.of(true, Optional.empty(), DEFAULT_LOCATION),
        Arguments.of(false, Optional.of(REQUESTED_LOCATION), DEFAULT_LOCATION));
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  static <T> ArgumentCaptor<T> createCaptor(Class<?> type) {
    return ArgumentCaptor.forClass((Class) type);
  }

  static BBjLoginStatus getStatus(PendingResult<BBjLoginStatus> result) throws Exception {
    CompletableFuture<BBjLoginStatus> future = new CompletableFuture<>();
    result.thenAccept(future::complete).exceptionally(error -> {
      future.completeExceptionally(error);

      return null;
    });

    return future.get(5, TimeUnit.SECONDS);
  }

  static <T> T getValue(PendingResult<T> result) throws Exception {
    CompletableFuture<T> future = new CompletableFuture<>();
    result.thenAccept(future::complete).exceptionally(error -> {
      future.completeExceptionally(error);

      return null;
    });

    return future.get(5, TimeUnit.SECONDS);
  }

  BBjAuthenticationService.Builder createBuilder() {
    return BBjAuthenticationService.builder().setGateway(gateway).setWorker(worker)
        .setLocationStore(locationStore).setSessionStore(sessionStore).setProperties(properties)
        .setConfiguration(configuration).setNavigator(navigator);
  }

  void captureHooks() {
    verify(worker).start(workCaptor.capture(), deliverCaptor.capture(), failCaptor.capture(),
        discardCaptor.capture());
  }

  List<String> getWarnings() {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == Level.WARNING)
        .map(formatter::formatMessage).toList();
  }

  List<String> getDebugMessages() {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == Level.FINE).map(formatter::formatMessage)
        .toList();
  }

  @SuppressWarnings("unchecked")
  private static Consumer<Location> createNavigator() {
    return mock(Consumer.class);
  }
}
