package com.webforj.spring.security.bearer;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.spring.security.WebforjSecurityContextHolderStrategy;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

class WebforjBearerTokenFilterTest {

  private static final String KEY =
      HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY;

  private final WebforjSecurityContextHolderStrategy holderStrategy =
      new WebforjSecurityContextHolderStrategy();
  private final WebforjBearerTokenFilter filter =
      new WebforjBearerTokenFilter(new WebforjBearerTokenResolver("jwt", request -> false),
          new WebforjBearerTokenContextRepository(request -> false), holderStrategy);
  private final MockHttpSession session = new MockHttpSession();
  private final List<ServletRequest> passedRequests = new ArrayList<>();
  private final List<Authentication> passedLogins = new ArrayList<>();

  @AfterEach
  void tearDown() {
    holderStrategy.clearContext();
  }

  @Nested
  class WithoutToken {

    @Test
    void shouldRemoveStoredBearerLogin() throws Exception {
      session.setAttribute(KEY,
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));
      MockHttpServletRequest request = createRequest();

      filter.doFilter(request, new MockHttpServletResponse(), createChain(false));

      assertNull(session.getAttribute(KEY));
      assertEquals(List.of(request), passedRequests);
    }

    @Test
    void shouldPassRequestWithoutCurrentBearerLogin() throws Exception {
      holderStrategy
          .setContext(new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER")));

      filter.doFilter(createRequest(), new MockHttpServletResponse(), createChain(false));

      assertEquals(1, passedLogins.size());
      assertNull(passedLogins.get(0));
    }

    @Test
    void shouldKeepCurrentFormLogin() throws Exception {
      Authentication login = UsernamePasswordAuthenticationToken.authenticated("ada", null, null);
      holderStrategy.setContext(new SecurityContextImpl(login));

      filter.doFilter(createRequest(), new MockHttpServletResponse(), createChain(false));

      assertEquals(List.of(login), passedLogins);
    }
  }

  @Nested
  class WithoutTokenOnUnreadableCookie {

    @Test
    void shouldKeepStoredBearerLogin() throws Exception {
      SecurityContext stored =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      session.setAttribute(KEY, stored);
      holderStrategy.setContext(stored);
      MockHttpServletRequest request = createRequest("POST");

      filter.doFilter(request, new MockHttpServletResponse(), createChain(false));

      assertSame(stored, session.getAttribute(KEY));
      assertEquals(List.of(stored.getAuthentication()), passedLogins);
    }

    @Test
    void shouldRemoveStoredBearerLoginOfExpiredToken() throws Exception {
      SecurityContext stored = new SecurityContextImpl(BearerTokenLogins.createExpired("ada"));
      session.setAttribute(KEY, stored);
      holderStrategy.setContext(stored);

      filter.doFilter(createRequest("POST"), new MockHttpServletResponse(), createChain(false));

      assertNull(session.getAttribute(KEY));
      assertEquals(1, passedLogins.size());
      assertNull(passedLogins.get(0));
    }
  }

  @Nested
  class WithToken {

    @Test
    void shouldKeepStoredBearerLoginWhileTokenIsChecked() throws Exception {
      SecurityContext stored =
          new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_USER"));
      session.setAttribute(KEY, stored);
      MockHttpServletRequest request = createRequest();
      request.setCookies(new Cookie("jwt", "valid-token"));

      filter.doFilter(request, new MockHttpServletResponse(), createChain(false));

      assertSame(stored, session.getAttribute(KEY));
      assertEquals(List.of(request), passedRequests);
    }

    @Test
    void shouldPassRequestWithoutCurrentBearerLogin() throws Exception {
      holderStrategy
          .setContext(new SecurityContextImpl(BearerTokenLogins.create("ada", "ROLE_ADMIN")));
      MockHttpServletRequest request = createRequest();
      request.setCookies(new Cookie("jwt", "valid-token"));

      filter.doFilter(request, new MockHttpServletResponse(), createChain(false));

      assertEquals(1, passedLogins.size());
      assertNull(passedLogins.get(0));
    }

    @Test
    void shouldPassRejectedCookieRequestAgainWithoutTokenCookie() throws Exception {
      MockHttpServletRequest request = createRequest();
      request.setCookies(new Cookie("jwt", "expired-token"));

      filter.doFilter(request, new MockHttpServletResponse(), createChain(true));

      assertEquals(2, passedRequests.size());
      assertSame(request, passedRequests.get(0));
      assertArrayEquals(new Cookie[0], ((HttpServletRequest) passedRequests.get(1)).getCookies());
    }
  }

  private MockHttpServletRequest createRequest() {
    return createRequest("GET");
  }

  private MockHttpServletRequest createRequest(String method) {
    MockHttpServletRequest request = new MockHttpServletRequest(method, "/account");
    request.setSession(session);
    return request;
  }

  private FilterChain createChain(boolean rejecting) {
    return (request, response) -> {
      passedRequests.add(request);
      passedLogins.add(holderStrategy.getContext().getAuthentication());
      if (rejecting) {
        request.setAttribute(WebforjBearerTokenFailureHandler.REJECTED_ATTRIBUTE_NAME,
            Boolean.TRUE);
      }
    };
  }
}
