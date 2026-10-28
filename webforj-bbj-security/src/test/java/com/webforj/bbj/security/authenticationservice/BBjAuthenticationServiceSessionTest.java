package com.webforj.bbj.security.authenticationservice;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.BBjAuthenticationService;
import com.webforj.bbj.security.SignInFixture;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BBjAuthenticationServiceSessionTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Nested
  class Construction {

    @ParameterizedTest
    @ValueSource(strings = {"gateway", "store", "properties", "configuration", "navigator"})
    void shouldRefuseMissingValueNamingIt(String name) {
      BBjAuthenticationService.Builder builder = fixture.createBuilder();
      switch (name) {
        case "gateway" -> builder.setGateway(null);
        case "store" -> builder.setStore(null);
        case "properties" -> builder.setProperties(null);
        case "configuration" -> builder.setConfiguration(null);
        default -> builder.setNavigator(null);
      }

      NullPointerException thrown = assertThrows(NullPointerException.class, builder::build);

      assertEquals(name, thrown.getMessage());
    }
  }

  @Nested
  class CurrentUser {

    @Test
    void shouldBeEmptyBeforeAnySignIn() {
      assertFalse(fixture.currentUser().isPresent());
    }
  }

  @Nested
  class RestoreSession {

    @Test
    void shouldPutTheStoredSessionInPlace() {
      when(fixture.store.readSession()).thenReturn(Optional.of(fixture.session));

      fixture.restoreSession();

      assertSame(fixture.principal, fixture.currentUser().orElseThrow());
    }

    @Test
    void shouldLeaveNobodySignedInWhenTheStoreIsEmpty() {
      when(fixture.store.readSession()).thenReturn(Optional.empty());

      fixture.restoreSession();

      assertFalse(fixture.currentUser().isPresent());
    }
  }
}
