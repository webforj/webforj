package com.webforj.bbj.security.store;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import jakarta.servlet.http.HttpSession;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjSessionStoreTest {

  @ParameterizedTest
  @MethodSource("accessors")
  void shouldCreateTheStoreThatFitsTheEnvironment(Optional<HttpSession.Accessor> accessor,
      Class<? extends BBjSessionStore> expected) {
    Environment environment = mock(Environment.class);
    when(environment.getSessionAccessor()).thenReturn(accessor);

    BBjSessionStore store = BBjSessionStore.create(environment);

    assertInstanceOf(expected, store);
  }

  private static Stream<Arguments> accessors() {
    return Stream.of(
        Arguments.of(Optional.of(mock(HttpSession.Accessor.class)), BBjHttpSessionStore.class),
        Arguments.of(Optional.empty(), BBjFileSessionStore.class));
  }
}
