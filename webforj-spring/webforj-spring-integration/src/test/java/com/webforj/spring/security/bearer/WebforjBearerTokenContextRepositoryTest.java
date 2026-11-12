package com.webforj.spring.security.bearer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.List;
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
    void shouldStoreNothingWhenSavingFrameworkRequestIntoSessionWithoutLogin() {
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
    void shouldStoreNothingWhenSavingFrameworkRequestOverBearerLoginOfAnotherUser() {
      WebforjBearerTokenContextRepository frameworkRepository =
          new WebforjBearerTokenContextRepository(request -> true);
      SecurityContext storedLogin =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      session.setAttribute(KEY, storedLogin);
      SecurityContext bearerLogin =
          new SecurityContextImpl(BearerTokenLogins.create("grace", "ROLE_USER"));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/account");
      request.setSession(session);
      String idBefore = session.getId();

      frameworkRepository.saveContext(bearerLogin, request, new MockHttpServletResponse());

      assertEquals(idBefore, request.getSession().getId());
      assertSame(storedLogin, request.getSession().getAttribute(KEY));
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
}
