package com.webforj.bbj.security;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static com.webforj.bbj.security.SignInFixture.createCaptor;
import static com.webforj.bbj.security.SignInFixture.getValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.router.history.Location;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.EnumSource.Mode;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;

class BBjAuthenticationServiceLogoutTest {
  private final SignInFixture fixture = new SignInFixture();
  private final PendingResult<Void> closePending = new PendingResult<>();
  private final ArgumentCaptor<Supplier<Boolean>> closeWorkCaptor = createCaptor(Supplier.class);
  private final ArgumentCaptor<Function<Boolean, Void>> closeDeliverCaptor =
      createCaptor(Function.class);
  private final ArgumentCaptor<Function<BBjAdminFailure, Void>> closeFailCaptor =
      createCaptor(Function.class);
  private final ArgumentCaptor<Consumer<Boolean>> closeDiscardCaptor = createCaptor(Consumer.class);

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  private void startSignedInSession() {
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    fixture.pending.complete(fixture.deliverCaptor.getValue().apply(fixture.session));
    clearInvocations(fixture.worker, fixture.navigator, fixture.locationStore, fixture.gateway);
    doReturn(closePending).when(fixture.worker).start(any(), any(), any(), any());
  }

  private void readCloseHooks() {
    verify(fixture.worker).start(closeWorkCaptor.capture(), closeDeliverCaptor.capture(),
        closeFailCaptor.capture(), closeDiscardCaptor.capture());
  }

  @Test
  void shouldCompleteWithNullAndCallNoOneWhenNobodySignedIn() throws Exception {
    PendingResult<Void> result = fixture.service.startLogout();

    assertTrue(result.isDone());
    assertNull(getValue(result));
    verifyNoInteractions(fixture.navigator, fixture.worker, fixture.gateway, fixture.locationStore);
  }

  @Test
  void shouldCancelTheRunningSignInAndNeverKeepItsSession() {
    fixture.service.startLogin("alice", PASSWORD);

    PendingResult<Void> result = fixture.service.startLogout();

    assertTrue(fixture.pending.isCancelled());
    assertFalse(fixture.pending.complete(BBjLoginStatus.AUTHENTICATED));
    assertTrue(result.isDone());
    assertFalse(fixture.service.currentUser().isPresent());
    verifyNoInteractions(fixture.navigator);
  }

  @Test
  void shouldForgetTheUserRightAway() {
    startSignedInSession();

    fixture.service.startLogout();

    assertFalse(fixture.service.currentUser().isPresent());
  }

  @Test
  void shouldClearTheSessionStore() {
    startSignedInSession();

    fixture.service.startLogout();

    verify(fixture.sessionStore).clearSession();
  }

  @Test
  void shouldClearTheLocationStore() {
    startSignedInSession();

    fixture.service.startLogout();

    verify(fixture.locationStore).clearLocation();
  }

  @ParameterizedTest
  @MethodSource("com.webforj.bbj.security.SignInFixture#getLogoutCases")
  void shouldOpenTheLogoutLocationOfTheConfiguration(String logoutPath, Location expected) {
    fixture.properties.setLogoutSuccessPath(logoutPath);
    startSignedInSession();

    fixture.service.startLogout();

    verify(fixture.navigator).accept(expected);
  }

  @Test
  void shouldCloseTheSignedInSession() {
    startSignedInSession();
    fixture.service.startLogout();
    readCloseHooks();

    closeWorkCaptor.getValue().get();

    verify(fixture.gateway).closeSession(fixture.session);
  }

  @Test
  void shouldCompleteWithNullAfterTheClose() throws Exception {
    startSignedInSession();
    PendingResult<Void> result = fixture.service.startLogout();
    readCloseHooks();
    closeWorkCaptor.getValue().get();
    assertFalse(result.isDone());

    closePending.complete(closeDeliverCaptor.getValue().apply(Boolean.TRUE));

    assertSame(closePending, result);
    assertNull(getValue(result));
  }

  @Test
  void shouldCompleteWithNullAndLogDebugWhenTheTokenWasRejected() throws Exception {
    startSignedInSession();
    PendingResult<Void> result = fixture.service.startLogout();
    readCloseHooks();
    BBjAdminFailure failure = new BBjAdminFailure(Kind.TOKEN_REJECTED, "ended");

    closePending.complete(closeFailCaptor.getValue().apply(failure));

    assertNull(getValue(result));
    assertEquals(List.of("The token was already ended: ended"), fixture.getDebugMessages());
    assertEquals(List.of(), fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
  void shouldCompleteWithNullAndWarnWhenTheTokenCouldNotBeEnded(Kind kind) throws Exception {
    startSignedInSession();
    PendingResult<Void> result = fixture.service.startLogout();
    readCloseHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

    closePending.complete(closeFailCaptor.getValue().apply(failure));

    assertNull(getValue(result));
    assertEquals(List.of("The token could not be ended and expires on its own: not served"),
        fixture.getWarnings());
    assertEquals(List.of(), fixture.getDebugMessages());
  }

  @Test
  void shouldActLikeNobodySignedInOnSecondLogout() throws Exception {
    startSignedInSession();
    fixture.service.startLogout();
    clearInvocations(fixture.worker, fixture.navigator, fixture.locationStore, fixture.gateway);

    PendingResult<Void> result = fixture.service.startLogout();

    assertTrue(result.isDone());
    assertNull(getValue(result));
    verifyNoInteractions(fixture.navigator, fixture.worker, fixture.gateway, fixture.locationStore);
  }
}
