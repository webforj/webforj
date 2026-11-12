package com.webforj.spring.security.bearer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

class WebforjBearerTokenContextRepositoryTest {

  private static final String KEY =
      HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY;

  private final WebforjBearerTokenContextRepository repository =
      new WebforjBearerTokenContextRepository(request -> false);
  private final MockHttpSession session = new MockHttpSession();

  @Nested
  class Saving {

    @Test
    void shouldSaveBearerLoginInSession() {
      SecurityContext context =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);

      repository.saveContext(context, request, new MockHttpServletResponse());

      assertSame(context, session.getAttribute(KEY));
    }

    @Test
    void shouldKeepStoredFormLoginWhenSaving() {
      SecurityContext formLogin = new SecurityContextImpl(UsernamePasswordAuthenticationToken
          .authenticated("ada", null, List.of(new SimpleGrantedAuthority("ROLE_USER"))));
      session.setAttribute(KEY, formLogin);
      SecurityContext bearerLogin =
          new SecurityContextImpl(BearerTokenLogins.create("grace", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);

      repository.saveContext(bearerLogin, request, new MockHttpServletResponse());

      assertSame(formLogin, session.getAttribute(KEY));
    }

    @Test
    void shouldReplaceStoredBearerLoginWhenSaving() {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
      SecurityContext bearerLogin =
          new SecurityContextImpl(BearerTokenLogins.create("grace", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);

      repository.saveContext(bearerLogin, request, new MockHttpServletResponse());

      assertSame(bearerLogin, session.getAttribute(KEY));
    }

    @Test
    void shouldChangeSessionIdWhenSavingIntoSessionWithoutLogin() {
      SecurityContext context =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      repository.saveContext(context, request, new MockHttpServletResponse());

      assertNotEquals(idBefore, request.getSession().getId());
      assertSame(context, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldStoreLoginWithoutNewSessionIdWhenSavingFrameworkRequestIntoNewSession() {
      WebforjBearerTokenContextRepository frameworkRepository =
          new WebforjBearerTokenContextRepository(request -> true);
      SecurityContext context =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      session.setNew(true);
      String idBefore = session.getId();

      frameworkRepository.saveContext(context, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertSame(context, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldChangeSessionIdWhenReplacingStoredBearerLoginOfAnotherUser() {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
      SecurityContext bearerLogin =
          new SecurityContextImpl(BearerTokenLogins.create("grace", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      repository.saveContext(bearerLogin, request, new MockHttpServletResponse());

      assertNotEquals(idBefore, request.getSession().getId());
      assertSame(bearerLogin, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldChangeSessionIdOnceWhenSavingLoginOfAnotherUserFromParallelRequests()
        throws InterruptedException, ExecutionException, TimeoutException {
      CountingSession countingSession = new CountingSession();
      countingSession.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
      SecurityContext firstLogin =
          new SecurityContextImpl(BearerTokenLogins.create("grace", "ROLE_USER"));
      SecurityContext secondLogin =
          new SecurityContextImpl(BearerTokenLogins.create("grace", "ROLE_USER"));
      MockHttpServletRequest firstRequest = new MockHttpServletRequest("GET", "/account");
      firstRequest.setSession(countingSession);
      MockHttpServletRequest secondRequest = new MockHttpServletRequest("GET", "/account");
      secondRequest.setSession(countingSession);
      CyclicBarrier barrier = new CyclicBarrier(2);
      WebforjBearerTokenContextRepository racingRepository =
          new WebforjBearerTokenContextRepository(request -> {
            try {
              barrier.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException | BrokenBarrierException | TimeoutException e) {
              return false;
            }

            return false;
          });
      ExecutorService executor = Executors.newFixedThreadPool(2);

      try {
        Future<?> first = executor.submit(() -> racingRepository.saveContext(firstLogin,
            firstRequest, new MockHttpServletResponse()));
        Future<?> second = executor.submit(() -> racingRepository.saveContext(secondLogin,
            secondRequest, new MockHttpServletResponse()));
        first.get(5, TimeUnit.SECONDS);
        second.get(5, TimeUnit.SECONDS);
      } finally {
        executor.shutdownNow();
      }

      Object stored = countingSession.getAttribute(KEY);
      assertEquals(1, countingSession.getChangeCount());
      assertTrue(stored == firstLogin || stored == secondLogin);
    }

    @Test
    void shouldChangeSessionIdWhenReplacingStoredBearerLoginWithoutName() {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.createWithoutName("ROLE_USER")));
      SecurityContext bearerLogin =
          new SecurityContextImpl(BearerTokenLogins.createWithoutName("ROLE_ADMIN"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      repository.saveContext(bearerLogin, request, new MockHttpServletResponse());

      assertNotEquals(idBefore, request.getSession().getId());
      assertSame(bearerLogin, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldChangeSessionIdWhenReplacingStoredBearerLoginWithoutNameWithOtherToken() {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.createWithoutName("ROLE_USER", "first-token")));
      SecurityContext bearerLogin =
          new SecurityContextImpl(BearerTokenLogins.createWithoutName("ROLE_USER", "second-token"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      repository.saveContext(bearerLogin, request, new MockHttpServletResponse());

      assertNotEquals(idBefore, request.getSession().getId());
      assertSame(bearerLogin, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldKeepSessionIdWhenRefreshingStoredBearerLoginWithoutNameWithSameToken() {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.createWithoutName("ROLE_USER")));
      SecurityContext refreshedLogin =
          new SecurityContextImpl(BearerTokenLogins.createWithoutName("ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      repository.saveContext(refreshedLogin, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertSame(refreshedLogin, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldKeepSessionIdWhenRefreshingStoredBearerLoginWithNullNameWithSameToken() {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.createWithNullName("opaque-token")));
      SecurityContext refreshedLogin =
          new SecurityContextImpl(BearerTokenLogins.createWithNullName("opaque-token"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      repository.saveContext(refreshedLogin, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertSame(refreshedLogin, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldKeepSessionIdWhenRefreshingStoredBearerLoginOfSameUser() {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
      SecurityContext refreshedLogin =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      repository.saveContext(refreshedLogin, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertSame(refreshedLogin, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldReplaceBearerLoginWithoutNewSessionIdWhenSavingFrameworkRequestIntoNewSession() {
      SecurityContext storedLogin =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      session.setAttribute(KEY, storedLogin);
      WebforjBearerTokenContextRepository frameworkRepository =
          new WebforjBearerTokenContextRepository(request -> true);
      SecurityContext bearerLogin =
          new SecurityContextImpl(BearerTokenLogins.create("grace", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      session.setNew(true);
      String idBefore = session.getId();

      frameworkRepository.saveContext(bearerLogin, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertSame(bearerLogin, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldRemoveStoredBearerLoginWhenSavingFrameworkRequestOfAnotherUserIntoJoinedSession() {
      WebforjBearerTokenContextRepository frameworkRepository =
          new WebforjBearerTokenContextRepository(request -> true);
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
      SecurityContext bearerLogin =
          new SecurityContextImpl(BearerTokenLogins.create("grace", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      frameworkRepository.saveContext(bearerLogin, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertNull(request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldStoreRefreshedLoginWhenSavingFrameworkRequestOfSameUserIntoJoinedSession() {
      WebforjBearerTokenContextRepository frameworkRepository =
          new WebforjBearerTokenContextRepository(request -> true);
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
      SecurityContext refreshedLogin =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      frameworkRepository.saveContext(refreshedLogin, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertSame(refreshedLogin, request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldStoreNothingWhenSavingFrameworkRequestIntoJoinedSessionWithoutLogin() {
      WebforjBearerTokenContextRepository frameworkRepository =
          new WebforjBearerTokenContextRepository(request -> true);
      SecurityContext context =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      frameworkRepository.saveContext(context, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertNull(request.getSession().getAttribute(KEY));
    }

    @Test
    void shouldCreateSessionWhenSavingWithoutSession() {
      SecurityContext context =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");

      repository.saveContext(context, request, new MockHttpServletResponse());

      assertSame(context, request.getSession(false).getAttribute(KEY));
    }
  }

  @Nested
  class Removing {

    @Test
    void shouldRemoveStoredBearerLogin() {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);

      repository.removeContext(request);

      assertNull(session.getAttribute(KEY));
    }

    @Test
    void shouldKeepStoredFormLogin() {
      SecurityContext context = new SecurityContextImpl(UsernamePasswordAuthenticationToken
          .authenticated("ada", null, List.of(new SimpleGrantedAuthority("ROLE_USER"))));
      session.setAttribute(KEY, context);
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);

      repository.removeContext(request);

      assertSame(context, session.getAttribute(KEY));
    }

    @Test
    void shouldCreateNoSessionWhenRemoving() {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");

      repository.removeContext(request);

      assertNull(request.getSession(false));
    }
  }

  private static final class CountingSession extends MockHttpSession {
    private final AtomicInteger changeCount = new AtomicInteger();

    int getChangeCount() {
      return changeCount.get();
    }

    @Override
    public String changeSessionId() {
      changeCount.incrementAndGet();
      return super.changeSessionId();
    }
  }
}
