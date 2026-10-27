package com.webforj.bbj.security.authenticationservice;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.bbj.security.BBjAuthenticationService;
import com.webforj.bbj.security.SignInFixture;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BBjAuthenticationServiceConstructionTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @ParameterizedTest
  @ValueSource(strings = {"gateway", "locationStore", "sessionStore", "properties", "configuration",
      "navigator"})
  void shouldRefuseMissingValueNamingIt(String name) {
    BBjAuthenticationService.Builder builder = fixture.createBuilder();
    switch (name) {
      case "gateway" -> builder.setGateway(null);
      case "locationStore" -> builder.setLocationStore(null);
      case "sessionStore" -> builder.setSessionStore(null);
      case "properties" -> builder.setProperties(null);
      case "configuration" -> builder.setConfiguration(null);
      default -> builder.setNavigator(null);
    }

    NullPointerException thrown = assertThrows(NullPointerException.class, builder::build);

    assertEquals(name, thrown.getMessage());
  }
}
