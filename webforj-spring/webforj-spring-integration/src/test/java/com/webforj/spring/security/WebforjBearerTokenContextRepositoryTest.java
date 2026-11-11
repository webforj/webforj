package com.webforj.spring.security;

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
      new WebforjBearerTokenContextRepository();
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
