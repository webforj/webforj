package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.Environment;
import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.component.Theme;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.component.toast.Toast;
import com.webforj.dispatcher.EventListener;
import com.webforj.environment.ObjectTable;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

class BBjSecurityTest {
  private static final String KEY = BBjRouteSecurityContext.class.getName();
  private static final Instant NOW = Instant.parse("2026-01-01T12:00:00Z");
  private final Clock clock = Clock.fixed(NOW, ZoneOffset.UTC);
  private final BBjAuthenticationState state = new BBjAuthenticationState();
  private MockedStatic<ObjectTable> mockedObjectTable;

  @BeforeEach
  void setUp() {
    mockedObjectTable = Mockito.mockStatic(ObjectTable.class);
  }

  @AfterEach
  void tearDown() {
    mockedObjectTable.close();
  }

  private void addContext() {
    mockedObjectTable.when(() -> ObjectTable.contains(KEY)).thenReturn(true);
    mockedObjectTable.when(() -> ObjectTable.get(KEY))
        .thenReturn(new BBjRouteSecurityContext(state, clock));
  }

  private void setAuthentication(Instant expiresAt) {
    state.setAuthentication(new BBjAuthentication("alice", List.of("admin"), expiresAt));
  }

  @Nested
  class Anonymous {

    @BeforeEach
    void setUp() {
      addContext();
    }

    @Test
    void shouldHaveNoCurrentUser() {
      assertTrue(BBjSecurity.currentUser().isEmpty());
    }

    @Test
    void shouldHaveNoRole() {
      assertFalse(BBjSecurity.hasRole("admin"));
    }

    @Test
    void shouldFailToRequireAuthentication() {
      SecurityException exception = assertThrows(SecurityException.class, BBjSecurity::require);

      assertEquals("Authentication is required, sign in and try again", exception.getMessage());
    }

    @Test
    void shouldFailToRequireRole() {
      SecurityException exception =
          assertThrows(SecurityException.class, () -> BBjSecurity.require("admin"));

      assertEquals("Authentication is required, sign in and try again", exception.getMessage());
    }
  }

  @Nested
  class Authenticated {

    @BeforeEach
    void setUp() {
      addContext();
      setAuthentication(NOW.plusSeconds(60));
    }

    @Test
    void shouldReturnTheCurrentUser() {
      BBjPrincipal principal = BBjSecurity.currentUser().orElseThrow();

      assertEquals("alice", principal.getUserName());
      assertEquals(Set.of("admin"), principal.getRoles());
    }

    @Test
    void shouldHaveTheRoleOfTheUserOnly() {
      assertTrue(BBjSecurity.hasRole("admin"));
      assertFalse(BBjSecurity.hasRole("staff"));
    }

    @Test
    void shouldRejectNullRole() {
      assertFalse(BBjSecurity.hasRole(null));
    }

    @Test
    void shouldReturnThePrincipalWhenAuthenticationIsRequired() {
      assertEquals("alice", BBjSecurity.require().getUserName());
    }

    @Test
    void shouldRejectNullRequiredRole() {
      assertThrows(NullPointerException.class, () -> BBjSecurity.require(null));
    }

    @Test
    void shouldReturnThePrincipalWhenTheRoleIsRequired() {
      assertEquals("alice", BBjSecurity.require("admin").getUserName());
    }

    @Test
    void shouldFailWhenTheRequiredRoleIsMissing() {
      SecurityException exception =
          assertThrows(SecurityException.class, () -> BBjSecurity.require("staff"));

      assertEquals("Access denied, the role staff is required", exception.getMessage());
    }

    @Test
    void shouldNeverHavePermissionYet() {
      assertFalse(BBjSecurity.hasPermission("admin"));
    }
  }

  @Nested
  class Expired {

    @BeforeEach
    void setUp() {
      addContext();
      setAuthentication(NOW);
    }

    @Test
    void shouldBehaveLikeAnonymous() {
      assertTrue(BBjSecurity.currentUser().isEmpty());
      assertFalse(BBjSecurity.hasRole("admin"));
      assertThrows(SecurityException.class, BBjSecurity::require);
    }
  }

  @Nested
  class WithoutContext {

    @BeforeEach
    void setUp() {
      mockedObjectTable.when(() -> ObjectTable.contains(KEY)).thenReturn(false);
      mockedObjectTable.when(() -> ObjectTable.get(KEY)).thenThrow(new NoSuchElementException());
    }

    @Test
    void shouldHaveNoCurrentUser() {
      assertTrue(BBjSecurity.currentUser().isEmpty());
    }

    @Test
    void shouldHaveNoRole() {
      assertFalse(BBjSecurity.hasRole("admin"));
    }

    @Test
    void shouldHaveNoPermission() {
      assertFalse(BBjSecurity.hasPermission("admin"));
    }

    @Test
    void shouldFailToRequireAuthentication() {
      assertThrows(SecurityException.class, BBjSecurity::require);
    }

    @Test
    void shouldFailToRequireRole() {
      assertThrows(SecurityException.class, () -> BBjSecurity.require("admin"));
    }

    @Test
    void shouldBehaveLikeAnonymousWhenNothingIsRegistered() {

      assertTrue(BBjSecurity.currentUser().isEmpty());
      assertFalse(BBjSecurity.hasRole("admin"));
      assertFalse(BBjSecurity.hasPermission("admin"));
      assertThrows(SecurityException.class, BBjSecurity::require);
      assertThrows(SecurityException.class, () -> BBjSecurity.require("admin"));
    }
  }

  @Nested
  class Login {
    private final String serviceKey = BBjAuthenticationService.class.getName();
    private MockedStatic<Environment> mockedEnvironment;

    @BeforeEach
    void setUp() {
      mockedEnvironment = Mockito.mockStatic(Environment.class);
    }

    @AfterEach
    void tearDown() {
      mockedEnvironment.close();
    }

    @Test
    void shouldDelegateToTheRegisteredService() {
      BBjAuthenticationService service = Mockito.mock(BBjAuthenticationService.class);
      PendingResult<BBjLoginStatus> expected =
          PendingResult.completedWith(BBjLoginStatus.AUTHENTICATED);
      Mockito.when(service.startLogin("alice", "pw")).thenReturn(expected);
      mockedEnvironment.when(Environment::getCurrent).thenReturn(Mockito.mock(Environment.class));
      mockedObjectTable.when(() -> ObjectTable.contains(serviceKey)).thenReturn(true);
      mockedObjectTable.when(() -> ObjectTable.get(serviceKey)).thenReturn(service);

      assertSame(expected, BBjSecurity.login("alice", "pw"));
    }

    @Test
    void shouldFailWithoutARegisteredService() {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(Mockito.mock(Environment.class));
      mockedObjectTable.when(() -> ObjectTable.contains(serviceKey)).thenReturn(false);

      IllegalStateException exception =
          assertThrows(IllegalStateException.class, () -> BBjSecurity.login("alice", "pw"));

      assertEquals("Sign-in is not available, route security is not enabled",
          exception.getMessage());
    }

    @Test
    void shouldFailOffTheEnvironmentThread() {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(null);

      IllegalStateException exception =
          assertThrows(IllegalStateException.class, () -> BBjSecurity.login("alice", "pw"));

      assertEquals("Sign-in must be called on the Environment thread of the app",
          exception.getMessage());
    }
  }

  @Nested
  class LoginDialog {
    private final String serviceKey = BBjAuthenticationService.class.getName();
    private MockedStatic<Environment> mockedEnvironment;
    private MockedStatic<Toast> mockedToast;
    private BBjAuthenticationService service;
    private com.webforj.component.login.Login dialog;
    private EventListener<LoginSubmitEvent> listener;

    @BeforeEach
    @SuppressWarnings("unchecked")
    void setUp() {
      mockedEnvironment = Mockito.mockStatic(Environment.class);
      mockedToast = Mockito.mockStatic(Toast.class);
      mockedEnvironment.when(Environment::getCurrent).thenReturn(Mockito.mock(Environment.class));
      service = Mockito.mock(BBjAuthenticationService.class);
      mockedObjectTable.when(() -> ObjectTable.contains(serviceKey)).thenReturn(true);
      mockedObjectTable.when(() -> ObjectTable.get(serviceKey)).thenReturn(service);

      dialog = Mockito.mock(com.webforj.component.login.Login.class);
      ArgumentCaptor<EventListener<LoginSubmitEvent>> captor =
          ArgumentCaptor.forClass(EventListener.class);
      BBjSecurity.login(dialog);
      Mockito.verify(dialog).onSubmit(captor.capture());
      listener = captor.getValue();
    }

    @AfterEach
    void tearDown() {
      mockedToast.close();
      mockedEnvironment.close();
    }

    private LoginSubmitEvent submitEvent() {
      LoginSubmitEvent event = Mockito.mock(LoginSubmitEvent.class);
      Mockito.when(event.getUsername()).thenReturn("alice");
      Mockito.when(event.getPassword()).thenReturn("pw");
      return event;
    }

    @Test
    void shouldCloseTheDialogOnAuthenticated() {
      Mockito.when(service.startLogin("alice", "pw"))
          .thenReturn(PendingResult.completedWith(BBjLoginStatus.AUTHENTICATED));

      listener.onEvent(submitEvent());

      Mockito.verify(dialog).close();
      Mockito.verify(dialog, Mockito.never()).setEnabled(true);
      Mockito.verify(dialog, Mockito.never()).setError(true);
    }

    @Test
    void shouldSetTheErrorOnInvalidCredentials() {
      Mockito.when(service.startLogin("alice", "pw"))
          .thenReturn(PendingResult.completedWith(BBjLoginStatus.INVALID_CREDENTIALS));

      listener.onEvent(submitEvent());

      Mockito.verify(dialog).setError(true);
      Mockito.verify(dialog).setEnabled(true);
      Mockito.verify(dialog, Mockito.never()).close();
    }

    @Test
    void shouldShowAWarningOnUnavailable() {
      Mockito.when(service.startLogin("alice", "pw"))
          .thenReturn(PendingResult.completedWith(BBjLoginStatus.UNAVAILABLE));

      listener.onEvent(submitEvent());

      mockedToast.verify(() -> Toast.show("Cannot sign in right now.", Theme.WARNING));
      Mockito.verify(dialog).setEnabled(true);
      Mockito.verify(dialog, Mockito.never()).setError(true);
      Mockito.verify(dialog, Mockito.never()).close();
    }

    @Test
    void shouldResetTheErrorBeforeTheLogin() {
      Mockito.when(service.startLogin("alice", "pw"))
          .thenReturn(PendingResult.completedWith(BBjLoginStatus.AUTHENTICATED));

      listener.onEvent(submitEvent());

      InOrder order = Mockito.inOrder(dialog, service);
      order.verify(dialog).setError(false);
      order.verify(service).startLogin("alice", "pw");
    }

    @Test
    void shouldStartALoginOnEverySubmit() {
      Mockito.when(service.startLogin("alice", "pw"))
          .thenReturn(PendingResult.completedWith(BBjLoginStatus.INVALID_CREDENTIALS));

      listener.onEvent(submitEvent());
      listener.onEvent(submitEvent());

      Mockito.verify(service, Mockito.times(2)).startLogin("alice", "pw");
      Mockito.verify(dialog, Mockito.times(2)).setError(false);
    }
  }
}
