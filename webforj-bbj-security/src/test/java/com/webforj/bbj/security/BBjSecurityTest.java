package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPermissions;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.component.Theme;
import com.webforj.component.login.Login;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.component.toast.Toast;
import com.webforj.environment.ObjectTable;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InOrder;
import org.mockito.MockedStatic;

class BBjSecurityTest {
  private static final String KEY = BBjAuthenticationService.class.getName();
  private final BBjPrincipal principal =
      new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of("admin-access"));
  private MockedStatic<ObjectTable> tableMock;
  private MockedStatic<Toast> toastMock;
  private BBjAuthenticationService service;

  @BeforeEach
  void setUp() {
    tableMock = mockStatic(ObjectTable.class);
    toastMock = mockStatic(Toast.class);
    service = mock(BBjAuthenticationService.class);
  }

  @AfterEach
  void tearDown() {
    toastMock.close();
    tableMock.close();
  }

  private void registerService() {
    tableMock.when(() -> ObjectTable.contains(KEY)).thenReturn(true);
    tableMock.when(() -> ObjectTable.get(KEY)).thenReturn(service);
  }

  private static Stream<Arguments> getSignInsWithProvider() {
    BiFunction<LoginSubmitEvent, Function<Boolean, String>, BBjLoginStatus> credentials =
        (event, provider) -> BBjSecurity.login("alice", "pw", provider);
    BiFunction<LoginSubmitEvent, Function<Boolean, String>, BBjLoginStatus> submitEvent =
        BBjSecurity::login;

    return Stream.of(Arguments.of(Named.of("user name and password", credentials)),
        Arguments.of(Named.of("submit event", submitEvent)));
  }

  private static Stream<Arguments> getSignInsWithoutProvider() {
    Function<LoginSubmitEvent, BBjLoginStatus> credentials =
        event -> BBjSecurity.login("alice", "pw");
    Function<LoginSubmitEvent, BBjLoginStatus> submitEvent = BBjSecurity::login;

    return Stream.of(Arguments.of(Named.of("user name and password", credentials)),
        Arguments.of(Named.of("submit event", submitEvent)));
  }

  @Nested
  class WithoutService {

    @Test
    void shouldGiveUnavailableForUserNameAndPassword() {
      assertEquals(BBjLoginStatus.UNAVAILABLE, BBjSecurity.login("alice", "pw"));
    }

    @Test
    void shouldGiveUnavailableForSubmitEvent() {
      LoginSubmitEvent event = mock(LoginSubmitEvent.class);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      var mockLogin = mock(Login.class);
      when(event.getComponent()).thenReturn(mockLogin);

      assertEquals(BBjLoginStatus.UNAVAILABLE, BBjSecurity.login(event));
    }

    @Test
    void shouldDoNothingOnLogout() {
      assertDoesNotThrow(BBjSecurity::logout);
    }

    @Test
    void shouldReturnEmptyCurrentUser() {
      assertTrue(BBjSecurity.currentUser().isEmpty());
    }

    @Test
    void shouldNotHaveRole() {
      assertFalse(BBjSecurity.hasRole("ROLE_USER"));
    }

    @Test
    void shouldNotHaveAuthority() {
      assertFalse(BBjSecurity.hasAuthority("admin-access"));
    }

    @Test
    void shouldRejectNullArguments() {
      LoginSubmitEvent event = null;

      assertThrows(NullPointerException.class, () -> BBjSecurity.login(null, "pw"));
      assertThrows(NullPointerException.class, () -> BBjSecurity.login("alice", null));
      assertThrows(NullPointerException.class, () -> BBjSecurity.login(event));
    }

    @Test
    void shouldRejectNullCodeProvider() {
      LoginSubmitEvent event = mock(LoginSubmitEvent.class);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      when(event.getComponent()).thenReturn(mock(Login.class));
      Function<Boolean, String> provider = null;

      assertThrows(NullPointerException.class, () -> BBjSecurity.login("alice", "pw", provider));
      assertThrows(NullPointerException.class, () -> BBjSecurity.login(event, provider));
    }
  }

  @Nested
  class WithService {

    @BeforeEach
    void setUp() {
      registerService();
    }

    @Test
    void shouldLookUpTheServiceUnderItsClassName() {
      BBjSecurity.currentUser();

      tableMock.verify(() -> ObjectTable.contains(KEY));
      tableMock.verify(() -> ObjectTable.get(KEY));
    }

    @Test
    void shouldReturnTheStatusOfTheServiceForUserNameAndPassword() {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.AUTHENTICATED);

      assertEquals(BBjLoginStatus.AUTHENTICATED, BBjSecurity.login("alice", "pw"));
    }

    @Test
    void shouldPassUserNameAndPasswordOfTheSubmitEvent() {
      LoginSubmitEvent event = mock(LoginSubmitEvent.class);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      var mockLogin = mock(Login.class);
      when(event.getComponent()).thenReturn(mockLogin);
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.INVALID_CREDENTIALS);

      assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, BBjSecurity.login(event));
    }

    @Test
    void shouldOpenTheTargetLocationOnAuthenticatedForUserNameAndPassword() {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.AUTHENTICATED);

      BBjSecurity.login("alice", "pw");

      InOrder order = inOrder(service);
      order.verify(service).login("alice", "pw", null);
      order.verify(service).openTargetLocation();
    }

    @ParameterizedTest
    @EnumSource(value = BBjLoginStatus.class, names = {"INVALID_CREDENTIALS", "UNAVAILABLE"})
    void shouldNotOpenTheTargetLocationOtherwiseForUserNameAndPassword(BBjLoginStatus status) {
      when(service.login("alice", "pw", null)).thenReturn(status);

      BBjSecurity.login("alice", "pw");

      verify(service, never()).openTargetLocation();
    }

    @Test
    void shouldSignOutThroughTheService() {
      BBjSecurity.logout();

      verify(service).logout();
    }

    @Test
    void shouldReturnThePrincipalOfTheService() {
      when(service.currentUser()).thenReturn(Optional.of(principal));

      assertEquals(Optional.of(principal), BBjSecurity.currentUser());
    }

    @Test
    void shouldReturnEmptyWhenTheServiceHasNoUser() {
      when(service.currentUser()).thenReturn(Optional.empty());

      assertTrue(BBjSecurity.currentUser().isEmpty());
    }

    @Test
    void shouldHaveTheRoleOfTheCurrentUser() {
      when(service.currentUser()).thenReturn(Optional.of(principal));

      assertTrue(BBjSecurity.hasRole("ROLE_USER"));
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {"ROLE_ADMIN", "role_user", "ROLE_USER "})
    void shouldNotHaveRoleTheCurrentUserDoesNotHold(String role) {
      when(service.currentUser()).thenReturn(Optional.of(principal));

      assertFalse(BBjSecurity.hasRole(role));
    }

    @Test
    void shouldNotHaveRoleWhenNobodyIsSignedIn() {
      when(service.currentUser()).thenReturn(Optional.empty());

      assertFalse(BBjSecurity.hasRole("ROLE_USER"));
    }

    @Test
    void shouldHaveTheAuthorityOfTheCurrentUser() {
      when(service.currentUser()).thenReturn(Optional.of(principal));

      assertTrue(BBjSecurity.hasAuthority("admin-access"));
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {"server-config", "ADMIN-ACCESS", "admin-access "})
    void shouldNotHaveAuthorityTheCurrentUserDoesNotHold(String authority) {
      when(service.currentUser()).thenReturn(Optional.of(principal));

      assertFalse(BBjSecurity.hasAuthority(authority));
    }

    @Test
    void shouldHaveThePermissionOfTheCurrentUser() {
      when(service.currentUser()).thenReturn(Optional.of(principal));

      assertTrue(BBjSecurity.hasAuthority(BBjPermissions.ADMIN_ACCESS));
    }

    @Test
    void shouldNotHavePermissionTheCurrentUserDoesNotHold() {
      when(service.currentUser()).thenReturn(Optional.of(principal));

      assertFalse(BBjSecurity.hasAuthority(BBjPermissions.SERVER_CONFIG));
    }

    @Test
    void shouldRejectNullPermission() {
      BBjPermissions permission = null;

      assertFalse(BBjSecurity.hasAuthority(permission));
    }

    @Test
    void shouldNotHaveAuthorityWhenNobodyIsSignedIn() {
      when(service.currentUser()).thenReturn(Optional.empty());

      assertFalse(BBjSecurity.hasAuthority("admin-access"));
    }

    @Test
    void shouldRejectNullArguments() {
      LoginSubmitEvent event = null;

      assertThrows(NullPointerException.class, () -> BBjSecurity.login(null, "pw"));
      assertThrows(NullPointerException.class, () -> BBjSecurity.login("alice", null));
      assertThrows(NullPointerException.class, () -> BBjSecurity.login(event));
    }
  }

  @Nested
  class SubmitEventWithService {
    private Login login;
    private LoginSubmitEvent event;

    @BeforeEach
    void setUp() {
      registerService();
      login = mock(Login.class);
      event = mock(LoginSubmitEvent.class);
      when(event.getComponent()).thenReturn(login);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.AUTHENTICATED);
    }

    @Test
    void shouldClearTheErrorBeforeTheSignIn() {
      BBjSecurity.login(event);

      InOrder order = inOrder(login, service);
      order.verify(login).setError(false);
      order.verify(service).login("alice", "pw", null);
    }

    @Test
    void shouldReturnTheStatusOfTheService() {
      assertEquals(BBjLoginStatus.AUTHENTICATED, BBjSecurity.login(event));
    }

    @Test
    void shouldSetTheErrorAndEnableTheFormOnInvalidCredentials() {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.INVALID_CREDENTIALS);

      BBjSecurity.login(event);

      verify(login).setError(true);
      verify(login).setEnabled(true);
    }

    @Test
    void shouldShowTheToastAndEnableTheFormOnUnavailable() {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.UNAVAILABLE);

      BBjSecurity.login(event);

      toastMock.verify(() -> Toast.show("Cannot sign in right now.", Theme.WARNING));
      verify(login).setEnabled(true);
    }

    @Test
    void shouldShowTheThrottledToastAndEnableTheFormOnThrottled() {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.THROTTLED);

      BBjSecurity.login(event);

      toastMock
          .verify(() -> Toast.show("Too many sign in attempts. Try again later.", Theme.WARNING));
      verify(login, never()).setError(true);
      verify(login).setEnabled(true);
      verify(service, never()).openTargetLocation();
    }

    @Test
    void shouldCloseTheFormWithoutEnablingItOnAuthenticated() {
      BBjSecurity.login(event);

      verify(login).close();
      verify(login, never()).setEnabled(true);
    }

    @Test
    void shouldCloseTheFormBeforeOpeningTheTargetLocation() {
      BBjSecurity.login(event);

      InOrder order = inOrder(login, service);
      order.verify(login).close();
      order.verify(service).openTargetLocation();
    }

    @ParameterizedTest
    @EnumSource(value = BBjLoginStatus.class, names = {"INVALID_CREDENTIALS", "UNAVAILABLE"})
    void shouldNotOpenTheTargetLocationOtherwise(BBjLoginStatus status) {
      when(service.login("alice", "pw", null)).thenReturn(status);

      BBjSecurity.login(event);

      verify(service, never()).openTargetLocation();
    }

    @Test
    void shouldEnableTheFormAndRethrowWhenTheServiceThrows() {
      IllegalStateException failure = new IllegalStateException("not available");
      when(service.login("alice", "pw", null)).thenThrow(failure);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> BBjSecurity.login(event));

      assertSame(failure, thrown);
      verify(login).setEnabled(true);
    }
  }

  @Nested
  class TwoFactor {
    private Login login;
    private LoginSubmitEvent event;
    private Function<Boolean, String> provider;

    @BeforeEach
    @SuppressWarnings("unchecked")
    void setUp() {
      registerService();
      login = mock(Login.class);
      event = mock(LoginSubmitEvent.class);
      when(event.getComponent()).thenReturn(login);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      provider = mock(Function.class);
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjSecurityTest#getSignInsWithProvider")
    void shouldNotAskForCodeWhenNoneIsRequired(
        BiFunction<LoginSubmitEvent, Function<Boolean, String>, BBjLoginStatus> signIn) {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.AUTHENTICATED);

      BBjLoginStatus status = signIn.apply(event, provider);

      assertEquals(BBjLoginStatus.AUTHENTICATED, status);
      verifyNoInteractions(provider);
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjSecurityTest#getSignInsWithProvider")
    void shouldSignInWithTheCodeOfTheFirstAsk(
        BiFunction<LoginSubmitEvent, Function<Boolean, String>, BBjLoginStatus> signIn) {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);
      when(service.login("alice", "pw", "111111")).thenReturn(BBjLoginStatus.AUTHENTICATED);
      when(provider.apply(false)).thenReturn("111111");

      BBjLoginStatus status = signIn.apply(event, provider);

      assertEquals(BBjLoginStatus.AUTHENTICATED, status);
      verify(provider, times(1)).apply(false);
      verify(provider, never()).apply(true);
      verify(service).openTargetLocation();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjSecurityTest#getSignInsWithProvider")
    void shouldAskAgainAfterRefusedCode(
        BiFunction<LoginSubmitEvent, Function<Boolean, String>, BBjLoginStatus> signIn) {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);
      when(service.login("alice", "pw", "111111")).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);
      when(service.login("alice", "pw", "222222")).thenReturn(BBjLoginStatus.AUTHENTICATED);
      when(provider.apply(false)).thenReturn("111111");
      when(provider.apply(true)).thenReturn("222222");

      BBjLoginStatus status = signIn.apply(event, provider);

      assertEquals(BBjLoginStatus.AUTHENTICATED, status);
      InOrder order = inOrder(provider, service);
      order.verify(service).login("alice", "pw", null);
      order.verify(provider).apply(false);
      order.verify(service).login("alice", "pw", "111111");
      order.verify(provider).apply(true);
      order.verify(service).login("alice", "pw", "222222");
      order.verify(service).openTargetLocation();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjSecurityTest#getSignInsWithProvider")
    void shouldStopWhenTheProviderGivesNoCode(
        BiFunction<LoginSubmitEvent, Function<Boolean, String>, BBjLoginStatus> signIn) {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);

      BBjLoginStatus status = signIn.apply(event, provider);

      assertEquals(BBjLoginStatus.TWO_FACTOR_REQUIRED, status);
      verify(provider, times(1)).apply(false);
      verify(service, times(1)).login(any(), any(), any());
      verify(service, never()).openTargetLocation();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjSecurityTest#getSignInsWithProvider")
    void shouldStopAskingWhenTheSignInIsThrottled(
        BiFunction<LoginSubmitEvent, Function<Boolean, String>, BBjLoginStatus> signIn) {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);
      when(service.login("alice", "pw", "111111")).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED)
          .thenReturn(BBjLoginStatus.THROTTLED);
      when(provider.apply(false)).thenReturn("111111");
      when(provider.apply(true)).thenReturn("111111");

      BBjLoginStatus status = signIn.apply(event, provider);

      assertEquals(BBjLoginStatus.THROTTLED, status);
      verify(provider, times(1)).apply(false);
      verify(provider, times(1)).apply(true);
      verify(service, times(3)).login(any(), any(), any());
      verify(service, never()).openTargetLocation();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjSecurityTest#getSignInsWithoutProvider")
    void shouldEndAsTwoFactorRequiredWithoutProvider(
        Function<LoginSubmitEvent, BBjLoginStatus> signIn) {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);

      BBjLoginStatus status = signIn.apply(event);

      assertEquals(BBjLoginStatus.TWO_FACTOR_REQUIRED, status);
      verify(service, times(1)).login(any(), any(), any());
      verify(service, never()).openTargetLocation();
    }

    @Test
    void shouldEnableTheFormWithoutErrorOrToastWhenNoCodeIsGiven() {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);

      BBjSecurity.login(event, provider);

      verify(login, never()).setError(true);
      verify(login).setEnabled(true);
      verify(login, never()).close();
      toastMock.verifyNoInteractions();
    }

    @Test
    void shouldCloseTheFormWithoutEnablingItAfterCorrectCode() {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);
      when(service.login("alice", "pw", "111111")).thenReturn(BBjLoginStatus.AUTHENTICATED);
      when(provider.apply(false)).thenReturn("111111");

      BBjSecurity.login(event, provider);

      InOrder order = inOrder(login, service);
      order.verify(login).close();
      order.verify(service).openTargetLocation();
      verify(login, never()).setEnabled(true);
      verify(login, never()).setError(true);
    }

    @Test
    void shouldShowTheThrottledToastAndEnableTheFormAfterRefusedCodes() {
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);
      when(service.login("alice", "pw", "111111")).thenReturn(BBjLoginStatus.THROTTLED);
      when(provider.apply(false)).thenReturn("111111");

      BBjSecurity.login(event, provider);

      toastMock
          .verify(() -> Toast.show("Too many sign in attempts. Try again later.", Theme.WARNING));
      verify(login, never()).setError(true);
      verify(login).setEnabled(true);
      verify(login, never()).close();
    }

    @Test
    void shouldEnableTheFormAndRethrowWhenTheProviderThrows() {
      IllegalStateException failure = new IllegalStateException("dialog closed");
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);
      when(provider.apply(false)).thenThrow(failure);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> BBjSecurity.login(event, provider));

      assertSame(failure, thrown);
      verify(login).setEnabled(true);
    }

    @Test
    void shouldEnableTheFormAndRethrowWhenTheSignInWithTheCodeThrows() {
      IllegalStateException failure = new IllegalStateException("not available");
      when(service.login("alice", "pw", null)).thenReturn(BBjLoginStatus.TWO_FACTOR_REQUIRED);
      when(service.login("alice", "pw", "111111")).thenThrow(failure);
      when(provider.apply(false)).thenReturn("111111");

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> BBjSecurity.login(event, provider));

      assertSame(failure, thrown);
      verify(login).setEnabled(true);
    }
  }
}
