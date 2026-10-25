package com.webforj.bbj.security;

import com.webforj.bbj.security.model.BBjLoginStatus;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.ComponentLifecycleObserver.LifecycleEvent;
import com.webforj.component.Theme;
import com.webforj.component.login.Login;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.component.toast.Toast;
import com.webforj.environment.ObjectTable;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
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
    void shouldCompleteWithUnavailableForUserNameAndPassword() {
      PendingResult<BBjLoginStatus> result = BBjSecurity.login("alice", "pw");

      AtomicReference<BBjLoginStatus> status = new AtomicReference<>();
      result.thenAccept(status::set);

      assertEquals(BBjLoginStatus.UNAVAILABLE, status.get());
    }

    @Test
    void shouldCompleteWithUnavailableForSubmitEvent() {
      LoginSubmitEvent event = mock(LoginSubmitEvent.class);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      when(event.getComponent()).thenReturn(mock(Login.class));

      PendingResult<BBjLoginStatus> result = BBjSecurity.login(event);

      AtomicReference<BBjLoginStatus> status = new AtomicReference<>();
      result.thenAccept(status::set);

      assertEquals(BBjLoginStatus.UNAVAILABLE, status.get());
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
    void shouldReturnTheResultOfTheServiceForUserNameAndPassword() {
      PendingResult<BBjLoginStatus> expected =
          PendingResult.completedWith(BBjLoginStatus.AUTHENTICATED);
      when(service.startLogin("alice", "pw")).thenReturn(expected);

      assertSame(expected, BBjSecurity.login("alice", "pw"));
    }

    @Test
    void shouldPassUserNameAndPasswordOfTheSubmitEvent() {
      LoginSubmitEvent event = mock(LoginSubmitEvent.class);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      when(event.getComponent()).thenReturn(mock(Login.class));
      PendingResult<BBjLoginStatus> expected =
          PendingResult.completedWith(BBjLoginStatus.INVALID_CREDENTIALS);
      when(service.startLogin("alice", "pw")).thenReturn(expected);

      assertSame(expected, BBjSecurity.login(event));
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
    private PendingResult<BBjLoginStatus> pending;

    @BeforeEach
    void setUp() {
      registerService();
      login = mock(Login.class);
      event = mock(LoginSubmitEvent.class);
      when(event.getComponent()).thenReturn(login);
      when(event.getUsername()).thenReturn("alice");
      when(event.getPassword()).thenReturn("pw");
      pending = new PendingResult<>();
      when(service.startLogin("alice", "pw")).thenReturn(pending);
    }

    private ComponentLifecycleObserver captureObserver() {
      ArgumentCaptor<ComponentLifecycleObserver> captor =
          ArgumentCaptor.forClass(ComponentLifecycleObserver.class);
      verify(login).addLifecycleObserver(captor.capture());

      return captor.getValue();
    }

    @Test
    void shouldClearTheErrorBeforeTheSignIn() {
      BBjSecurity.login(event);

      InOrder order = inOrder(login, service);
      order.verify(login).setError(false);
      order.verify(service).startLogin("alice", "pw");
    }

    @Test
    void shouldReturnTheResultOfTheService() {
      assertSame(pending, BBjSecurity.login(event));
    }

    @Test
    void shouldSetTheErrorAndEnableTheFormOnInvalidCredentials() {
      BBjSecurity.login(event);
      pending.complete(BBjLoginStatus.INVALID_CREDENTIALS);

      verify(login).setError(true);
      verify(login).setEnabled(true);
    }

    @Test
    void shouldShowTheToastAndEnableTheFormOnUnavailable() {
      BBjSecurity.login(event);
      pending.complete(BBjLoginStatus.UNAVAILABLE);

      toastMock.verify(() -> Toast.show("Cannot sign in right now.", Theme.WARNING));
      verify(login).setEnabled(true);
    }

    @Test
    void shouldCloseTheFormWithoutEnablingItOnAuthenticated() {
      BBjSecurity.login(event);
      pending.complete(BBjLoginStatus.AUTHENTICATED);

      verify(login).close();
      verify(login, never()).setEnabled(true);
    }

    @Test
    void shouldEnableTheFormAndRethrowWhenTheServiceThrows() {
      IllegalStateException failure = new IllegalStateException("not available");
      when(service.startLogin("alice", "pw")).thenThrow(failure);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> BBjSecurity.login(event));

      assertSame(failure, thrown);
      verify(login).setEnabled(true);
    }

    @Test
    void shouldCancelTheRunningResultOnDestroy() {
      BBjSecurity.login(event);

      captureObserver().onComponentLifecycleEvent(login, LifecycleEvent.DESTROY);

      assertTrue(pending.isCancelled());
    }

    @Test
    void shouldKeepTheRunningResultOnAnotherLifecycleEvent() {
      BBjSecurity.login(event);

      captureObserver().onComponentLifecycleEvent(login, LifecycleEvent.CREATE);

      assertFalse(pending.isCancelled());
    }

    @Test
    void shouldRemoveTheObserverOnceTheResultCompletes() {
      BBjSecurity.login(event);
      ComponentLifecycleObserver observer = captureObserver();

      pending.complete(BBjLoginStatus.AUTHENTICATED);

      verify(login).removeLifecycleObserver(observer);
    }
  }
}
