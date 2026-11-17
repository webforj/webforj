package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import com.webforj.bbj.security.BBjAdminFailure;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.SignInFixture;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.router.history.Location;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class BBjAuthenticationServicePendingTest {
  private static final Instant START = Instant.parse("2026-01-01T12:00:00Z");
  private final SignInFixture fixture = new SignInFixture();
  private final AtomicReference<Instant> now = new AtomicReference<>(START);
  private final Clock clock = new Clock() {
    @Override
    public ZoneId getZone() {
      return ZoneOffset.UTC;
    }

    @Override
    public Clock withZone(ZoneId zone) {
      return this;
    }

    @Override
    public Instant instant() {
      return now.get();
    }
  };

  @BeforeEach
  void setUpPending() {
    fixture.properties.setPendingSeconds(300);
    fixture.setService(fixture.createBuilder().setClock(clock).build());
  }

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  private static Stream<Arguments> getEndingActions() {
    Consumer<SignInFixture> cancel = SignInFixture::clearPending;
    Consumer<SignInFixture> logout = SignInFixture::logout;
    Consumer<SignInFixture> login = it -> it.login("bob", "other-pw");

    return Stream.of(Arguments.of(Named.of("cancelLogin", cancel)),
        Arguments.of(Named.of("logout", logout)), Arguments.of(Named.of("a new login", login)));
  }

  private void startPending() {
    fixture.savePendingLogin("alice", PASSWORD);
  }

  private void refuseCode(String code) {
    fixture.whenOpenSession("alice", PASSWORD, code)
        .thenThrow(new BBjAdminFailure(Kind.INVALID_CODE, "code refused"));
  }

  @Test
  void shouldRefuseNullCode() {
    String code = null;

    assertThrows(NullPointerException.class, () -> fixture.sendCode(code));
  }

  @Test
  void shouldSendTheKeptUserNameAndPasswordWithTheCode() {
    startPending();
    fixture.whenOpenSession("alice", PASSWORD, "123456").thenReturn(fixture.session);

    BBjLoginStatus status = fixture.sendCode("123456");

    assertEquals(BBjLoginStatus.AUTHENTICATED, status);
    fixture.verifyOpenSession(times(1), "alice", PASSWORD, "123456");
    verify(fixture.store).saveSession(fixture.session);
    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
  }

  @Test
  void shouldGiveCodeExpiredAndCallNoGatewayWhenNothingIsPending() {
    BBjLoginStatus status = fixture.sendCode("123456");

    assertEquals(BBjLoginStatus.CODE_EXPIRED, status);
    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldGiveCodeExpiredAndCallNoGatewayAfterSuccess() {
    startPending();
    fixture.whenOpenSession("alice", PASSWORD, "123456").thenReturn(fixture.session);
    fixture.sendCode("123456");
    clearInvocations(fixture.gateway);

    BBjLoginStatus status = fixture.sendCode("123456");

    assertEquals(BBjLoginStatus.CODE_EXPIRED, status);
    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldGiveTwoFactorRequiredForFourRefusedCodesAndCodeExpiredForTheFifth() {
    startPending();
    refuseCode("111111");

    for (var attempt = 1; attempt <= 4; attempt++) {
      assertEquals(BBjLoginStatus.TWO_FACTOR_REQUIRED, fixture.sendCode("111111"));
    }

    assertEquals(BBjLoginStatus.CODE_EXPIRED, fixture.sendCode("111111"));
    fixture.verifyOpenSession(times(5), "alice", PASSWORD, "111111");
  }

  @Test
  void shouldCallNoGatewayAfterTheFifthRefusedCode() {
    startPending();
    refuseCode("111111");
    for (var attempt = 1; attempt <= 5; attempt++) {
      fixture.sendCode("111111");
    }
    clearInvocations(fixture.gateway);

    BBjLoginStatus status = fixture.sendCode("111111");

    assertEquals(BBjLoginStatus.CODE_EXPIRED, status);
    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldCountRefusedCodesFromZeroForNewPendingSignIn() {
    startPending();
    refuseCode("111111");
    for (var attempt = 1; attempt <= 4; attempt++) {
      fixture.sendCode("111111");
    }

    startPending();

    assertEquals(BBjLoginStatus.TWO_FACTOR_REQUIRED, fixture.sendCode("111111"));
  }

  @Test
  void shouldUseThePendingSignInBeforeItsLifetimePassed() {
    startPending();
    fixture.whenOpenSession("alice", PASSWORD, "123456").thenReturn(fixture.session);
    now.set(START.plus(Duration.ofSeconds(299)));

    assertEquals(BBjLoginStatus.AUTHENTICATED, fixture.sendCode("123456"));
  }

  @Test
  void shouldGiveCodeExpiredAndCallNoGatewayWhenTheLifetimePassed() {
    startPending();
    now.set(START.plus(Duration.ofSeconds(300)));

    BBjLoginStatus status = fixture.sendCode("123456");

    assertEquals(BBjLoginStatus.CODE_EXPIRED, status);
    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldMeasureTheLifetimeFromTheConfiguredSeconds() {
    fixture.properties.setPendingSeconds(10);
    startPending();
    now.set(START.plus(Duration.ofSeconds(10)));

    assertEquals(BBjLoginStatus.CODE_EXPIRED, fixture.sendCode("123456"));
  }

  @ParameterizedTest
  @MethodSource("getEndingActions")
  void shouldEndThePendingSignInOnAction(Consumer<SignInFixture> action) {
    fixture.whenOpenSession("bob", "other-pw")
        .thenThrow(new BBjAdminFailure(Kind.INVALID_CREDENTIALS, "refused"));
    startPending();
    action.accept(fixture);
    clearInvocations(fixture.gateway);

    BBjLoginStatus status = fixture.sendCode("123456");

    assertEquals(BBjLoginStatus.CODE_EXPIRED, status);
    fixture.verifyOpenSession(never(), "alice", PASSWORD, "123456");
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, names = {"INVALID_CREDENTIALS", "UNAVAILABLE"})
  void shouldEndThePendingSignInOnAnyOtherStatusOfTheCodeStep(Kind kind) {
    startPending();
    fixture.whenOpenSession("alice", PASSWORD, "123456").thenThrow(new BBjAdminFailure(kind, "no"));
    fixture.sendCode("123456");
    clearInvocations(fixture.gateway);

    BBjLoginStatus status = fixture.sendCode("123456");

    assertEquals(BBjLoginStatus.CODE_EXPIRED, status);
    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldSendTheLatestPendingSignInWhenItIsReplaced() {
    startPending();
    fixture.savePendingLogin("bob", "other-pw");
    fixture.whenOpenSession("bob", "other-pw", "123456").thenReturn(fixture.session);

    fixture.sendCode("123456");

    fixture.verifyOpenSession(times(1), "bob", "other-pw", "123456");
    fixture.verifyOpenSession(never(), "alice", PASSWORD, "123456");
  }

  @Test
  void shouldKeepThePendingSignInOutOfTheStore() {
    startPending();
    refuseCode("111111");

    fixture.sendCode("111111");

    verifyNoInteractions(fixture.store);
  }

  @Test
  void shouldKeepThePasswordOutOfEveryLogRecord() {
    startPending();
    fixture.whenOpenSession("alice", PASSWORD, "123456")
        .thenThrow(new BBjAdminFailure(Kind.UNAVAILABLE, "not served"));
    fixture.sendCode("123456");
    fixture.sendCode("123456");

    SimpleFormatter formatter = new SimpleFormatter();
    assertTrue(fixture.records.stream().noneMatch(it -> formatter.format(it).contains(PASSWORD)));
  }

  @Test
  void shouldOpenTheCodeLocationOfTheConfiguration() {
    fixture.properties.setCodePath("/sign-in/code");

    fixture.openCodeLocation();

    verify(fixture.navigator).accept(new Location("/sign-in/code"));
  }

  @Test
  void shouldOpenNoLocationWhenNoCodePathIsSet() {
    fixture.openCodeLocation();

    verifyNoInteractions(fixture.navigator);
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldHaveCodeLocationOnlyWhenCodePathIsSet(boolean set) {
    fixture.properties.setCodePath(set ? "/sign-in/code" : null);

    assertEquals(set, fixture.hasCodeLocation());
  }
}
