package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.component.Theme;
import com.webforj.component.login.Login;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.component.toast.Toast;
import com.webforj.environment.ObjectTable;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InOrder;
import org.mockito.MockedStatic;

class BBjSecurityTest {
  private static final String KEY = BBjAuthenticationService.class.getName();
  private final BBjPrincipal principal = new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of());
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
    void shouldRejectNullArguments() {
      LoginSubmitEvent event = null;

      assertThrows(NullPointerException.class, () -> BBjSecurity.login(null, "pw"));
      assertThrows(NullPointerException.class, () -> BBjSecurity.login("alice", null));
      assertThrows(NullPointerException.class, () -> BBjSecurity.login(event));
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
      when(service.login("alice", "pw")).thenReturn(BBjLoginStatus.AUTHENTICATED);

      assertEquals(BBjLoginStatus.AUTHENTICATED, BBjSecurity.login("alice", "pw"));
    }

    @Test
    void shouldPassUserNameAndPasswordOfTheSubmitEvent() {
      LoginSubmitEvent event = mock(LoginSubmitEvent.class);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      var mockLogin = mock(Login.class);
      when(event.getComponent()).thenReturn(mockLogin);
      when(service.login("alice", "pw")).thenReturn(BBjLoginStatus.INVALID_CREDENTIALS);

      assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, BBjSecurity.login(event));
    }

    @Test
    void shouldOpenTheTargetLocationOnAuthenticatedForUserNameAndPassword() {
      when(service.login("alice", "pw")).thenReturn(BBjLoginStatus.AUTHENTICATED);

      BBjSecurity.login("alice", "pw");

      InOrder order = inOrder(service);
      order.verify(service).login("alice", "pw");
      order.verify(service).openTargetLocation();
    }

    @ParameterizedTest
    @EnumSource(value = BBjLoginStatus.class, names = {"INVALID_CREDENTIALS", "UNAVAILABLE"})
    void shouldNotOpenTheTargetLocationOtherwiseForUserNameAndPassword(BBjLoginStatus status) {
      when(service.login("alice", "pw")).thenReturn(status);

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
      when(service.login("alice", "pw")).thenReturn(BBjLoginStatus.AUTHENTICATED);
    }

    @Test
    void shouldClearTheErrorBeforeTheSignIn() {
      BBjSecurity.login(event);

      InOrder order = inOrder(login, service);
      order.verify(login).setError(false);
      order.verify(service).login("alice", "pw");
    }

    @Test
    void shouldReturnTheStatusOfTheService() {
      assertEquals(BBjLoginStatus.AUTHENTICATED, BBjSecurity.login(event));
    }

    @Test
    void shouldSetTheErrorAndEnableTheFormOnInvalidCredentials() {
      when(service.login("alice", "pw")).thenReturn(BBjLoginStatus.INVALID_CREDENTIALS);

      BBjSecurity.login(event);

      verify(login).setError(true);
      verify(login).setEnabled(true);
    }

    @Test
    void shouldShowTheToastAndEnableTheFormOnUnavailable() {
      when(service.login("alice", "pw")).thenReturn(BBjLoginStatus.UNAVAILABLE);

      BBjSecurity.login(event);

      toastMock.verify(() -> Toast.show("Cannot sign in right now.", Theme.WARNING));
      verify(login).setEnabled(true);
    }

    @Test
    void shouldShowTheThrottledToastAndEnableTheFormOnThrottled() {
      when(service.login("alice", "pw")).thenReturn(BBjLoginStatus.THROTTLED);

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
      when(service.login("alice", "pw")).thenReturn(status);

      BBjSecurity.login(event);

      verify(service, never()).openTargetLocation();
    }

    @Test
    void shouldEnableTheFormAndRethrowWhenTheServiceThrows() {
      IllegalStateException failure = new IllegalStateException("not available");
      when(service.login("alice", "pw")).thenThrow(failure);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> BBjSecurity.login(event));

      assertSame(failure, thrown);
      verify(login).setEnabled(true);
    }
  }
}
