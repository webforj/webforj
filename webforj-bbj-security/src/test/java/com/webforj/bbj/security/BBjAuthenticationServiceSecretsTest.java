package com.webforj.bbj.security;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbj.security.BBjAdminFailure.Kind;
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
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

    fixture.pending.complete(fixture.failCaptor.getValue().apply(failure));
    fixture.discardCaptor.getValue().accept(fixture.session);

    SimpleFormatter formatter = new SimpleFormatter();
    assertTrue(fixture.records.stream().noneMatch(it -> formatter.format(it).contains(PASSWORD)));
  }
}
