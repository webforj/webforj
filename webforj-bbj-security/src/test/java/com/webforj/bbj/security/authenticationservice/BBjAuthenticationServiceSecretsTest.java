package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbj.security.BBjAdminFailure;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.SignInFixture;
import java.util.logging.SimpleFormatter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class BBjAuthenticationServiceSecretsTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @ParameterizedTest
  @EnumSource(Kind.class)
  void shouldKeepThePasswordOutOfEveryLogRecord(Kind kind) {
    fixture.whenOpenSession("alice", PASSWORD).thenThrow(new BBjAdminFailure(kind, "not served"));

    fixture.login("alice", PASSWORD);

    SimpleFormatter formatter = new SimpleFormatter();
    assertTrue(fixture.records.stream().noneMatch(it -> formatter.format(it).contains(PASSWORD)));
  }
}
