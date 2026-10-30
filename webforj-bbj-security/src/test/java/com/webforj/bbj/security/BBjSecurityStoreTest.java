package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import jakarta.servlet.http.HttpSession;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class BBjSecurityStoreTest {
  private static final String SESSION_KEY = "webforj-bbj-session";
  private static final String LOCATION_KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;
  private static final Instant NOW = Instant.parse("2026-01-01T00:00:00Z");

  private MockedStatic<Environment> environmentMock;
  private Environment environment;
  private Map<String, Object> attributes;
  private BBjSecurityStore store;
  private BBjSession session;

  @BeforeEach
  @SuppressWarnings("unchecked")
  void setUp() {
    attributes = new HashMap<>();
    HttpSession httpSession = mock(HttpSession.class);
    when(httpSession.getAttribute(anyString()))
        .thenAnswer(call -> attributes.get(call.<String>getArgument(0)));
    doAnswer(call -> attributes.put(call.getArgument(0), call.getArgument(1))).when(httpSession)
        .setAttribute(anyString(), any());
    doAnswer(call -> attributes.remove(call.<String>getArgument(0))).when(httpSession)
        .removeAttribute(anyString());
    HttpSession.Accessor accessor = mock(HttpSession.Accessor.class);
    doAnswer(call -> {
      call.<Consumer<HttpSession>>getArgument(0).accept(httpSession);
      return null;
    }).when(accessor).access(any(Consumer.class));
    environment = mock(Environment.class);
    when(environment.getSessionAccessor()).thenReturn(Optional.of(accessor));
    environmentMock = mockStatic(Environment.class);
    environmentMock.when(Environment::getCurrent).thenReturn(environment);
    store = new BBjSecurityStore(Clock.fixed(NOW, ZoneOffset.UTC));
    session = new BBjSession(new BBjPrincipal("alice", Set.of("ADMIN"), Set.of("read")),
        "token-secret-123", NOW.plusSeconds(3600));
  }

  @AfterEach
  void tearDown() {
    environmentMock.close();
  }

  @Nested
  class SaveSession {

    @Test
    void shouldKeepTheSessionInTheHttpSession() {
      store.saveSession(session);

      assertSame(session, attributes.get(SESSION_KEY));
    }

    @Test
    void shouldReplaceTheSessionSavedBefore() {
      BBjSession later =
          new BBjSession(session.getPrincipal(), "token-later", NOW.plusSeconds(7200));
      store.saveSession(session);

      store.saveSession(later);

      assertSame(later, attributes.get(SESSION_KEY));
    }

    @Test
    void shouldAcceptTheConstructorWithTheSystemClock() {
      BBjSession later = new BBjSession(session.getPrincipal(), "token-secret-123",
          Instant.now().plusSeconds(3600));
      BBjSecurityStore system = new BBjSecurityStore();

      system.saveSession(later);

      assertSame(later, system.readSession().orElseThrow());
    }
  }

  @Nested
  class ReadSession {

    @Test
    void shouldReadTheSavedSession() {
      attributes.put(SESSION_KEY, session);

      assertSame(session, store.readSession().orElseThrow());
    }

    @Test
    void shouldReadNothingWhenNothingIsStored() {
      assertTrue(store.readSession().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("expiries")
    void shouldRemoveAnExpiredSession(Instant expiresAt) {
      attributes.put(SESSION_KEY, new BBjSession(session.getPrincipal(), "token-old", expiresAt));

      assertTrue(store.readSession().isEmpty());

      assertFalse(attributes.containsKey(SESSION_KEY));
    }

    @Test
    void shouldRemoveValueThatIsNotSession() {
      attributes.put(SESSION_KEY, "not a session");

      assertTrue(store.readSession().isEmpty());

      assertFalse(attributes.containsKey(SESSION_KEY));
    }

    private static Stream<Arguments> expiries() {
      return Stream.of(Arguments.of(NOW), Arguments.of(NOW.minusSeconds(1)));
    }
  }

  @Nested
  class ClearSession {

    @Test
    void shouldRemoveTheSession() {
      attributes.put(SESSION_KEY, session);

      store.clearSession();

      assertFalse(attributes.containsKey(SESSION_KEY));
    }
  }

  @Nested
  class ReadLocation {

    @Test
    void shouldReadTheSavedLocation() {
      attributes.put(LOCATION_KEY, "/protected?x=1");

      Optional<Location> location = store.readLocation();

      assertEquals("/protected?x=1", location.orElseThrow().getFullURI());
      assertTrue(attributes.containsKey(LOCATION_KEY));
    }

    @Test
    void shouldReadNothingWhenNoLocationIsSaved() {
      assertTrue(store.readLocation().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("invalidValues")
    void shouldDiscardValueThatIsNotPathInsideTheApp(Object value) {
      attributes.put(LOCATION_KEY, value);

      assertTrue(store.readLocation().isEmpty());

      assertFalse(attributes.containsKey(LOCATION_KEY));
    }

    private static Stream<Arguments> invalidValues() {
      return Stream.of(Arguments.of("//evil"), Arguments.of("/evil\\path"),
          Arguments.of("protected"), Arguments.of("https://evil.example"), Arguments.of(""),
          Arguments.of("/x\u0001"), Arguments.of(42));
    }
  }

  @Nested
  class ClearLocation {

    @Test
    void shouldClearTheSavedLocation() {
      attributes.put(LOCATION_KEY, "/protected");

      store.clearLocation();

      assertFalse(attributes.containsKey(LOCATION_KEY));
    }
  }

  @Nested
  class UnavailableHttpSession {

    @ParameterizedTest
    @MethodSource("unavailableAccessors")
    void shouldFailToSave(Optional<HttpSession.Accessor> accessor) {
      when(environment.getSessionAccessor()).thenReturn(accessor);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> store.saveSession(session));

      assertEquals("The sign in could not be kept", thrown.getMessage());
      assertNull(thrown.getCause());
    }

    @ParameterizedTest
    @MethodSource("unavailableAccessors")
    void shouldReadNoSession(Optional<HttpSession.Accessor> accessor) {
      when(environment.getSessionAccessor()).thenReturn(accessor);

      assertTrue(store.readSession().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("unavailableAccessors")
    void shouldNotThrowOnClearSession(Optional<HttpSession.Accessor> accessor) {
      when(environment.getSessionAccessor()).thenReturn(accessor);

      assertDoesNotThrow(store::clearSession);
    }

    @ParameterizedTest
    @MethodSource("unavailableAccessors")
    void shouldReadNoLocation(Optional<HttpSession.Accessor> accessor) {
      when(environment.getSessionAccessor()).thenReturn(accessor);

      assertTrue(store.readLocation().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("unavailableAccessors")
    void shouldNotThrowOnClearLocation(Optional<HttpSession.Accessor> accessor) {
      when(environment.getSessionAccessor()).thenReturn(accessor);

      assertDoesNotThrow(store::clearLocation);
    }

    private static Stream<Arguments> unavailableAccessors() {
      HttpSession.Accessor failing = mock(HttpSession.Accessor.class);
      doThrow(new IllegalStateException("secret-text")).when(failing).access(any());

      return Stream.of(Arguments.of(Optional.empty()), Arguments.of(Optional.of(failing)));
    }
  }
}
