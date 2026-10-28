package com.webforj.bbj.security.store;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import jakarta.servlet.http.HttpSession;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
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

class BBjHttpSessionRequestedLocationStoreTest {
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  private MockedStatic<Environment> environmentMock;
  private Environment environment;
  private Map<String, Object> attributes;
  private BBjHttpSessionRequestedLocationStore store;

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
    store = new BBjHttpSessionRequestedLocationStore();
  }

  @AfterEach
  void tearDown() {
    environmentMock.close();
  }

  @Nested
  class SaveLocation {

    @Test
    void shouldSaveTheFullUriUnderTheRequestedLocationKey() {
      store.saveLocation(new Location("/protected?x=1#top"));

      assertEquals("/protected?x=1#top", attributes.get(KEY));
    }
  }

  @Nested
  class ReadLocation {

    @Test
    void shouldReadTheSavedLocation() {
      attributes.put(KEY, "/protected?x=1");

      Optional<Location> location = store.readLocation();

      assertEquals("/protected?x=1", location.orElseThrow().getFullURI());
      assertTrue(attributes.containsKey(KEY));
    }

    @Test
    void shouldReadNothingWhenNoLocationIsSaved() {
      assertTrue(store.readLocation().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("invalidValues")
    void shouldDiscardValueThatIsNotPathInsideTheApp(Object value) {
      attributes.put(KEY, value);

      assertTrue(store.readLocation().isEmpty());

      assertFalse(attributes.containsKey(KEY));
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
      attributes.put(KEY, "/protected");

      store.clearLocation();

      assertFalse(attributes.containsKey(KEY));
    }
  }

  @Nested
  class UnavailableHttpSession {

    @ParameterizedTest
    @MethodSource("unavailableAccessors")
    void shouldNotThrowOnSave(Optional<HttpSession.Accessor> accessor) {
      when(environment.getSessionAccessor()).thenReturn(accessor);
      Location location = new Location("/protected");

      assertDoesNotThrow(() -> store.saveLocation(location));
    }

    @ParameterizedTest
    @MethodSource("unavailableAccessors")
    void shouldReadNothing(Optional<HttpSession.Accessor> accessor) {
      when(environment.getSessionAccessor()).thenReturn(accessor);

      assertTrue(store.readLocation().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("unavailableAccessors")
    void shouldNotThrowOnClear(Optional<HttpSession.Accessor> accessor) {
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
