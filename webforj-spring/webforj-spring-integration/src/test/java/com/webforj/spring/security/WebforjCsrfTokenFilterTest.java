package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import jakarta.servlet.http.HttpSession;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.web.csrf.CsrfToken;
import org.springframework.security.web.csrf.DefaultCsrfToken;
import org.springframework.security.web.csrf.DeferredCsrfToken;

@ExtendWith(MockitoExtension.class)
class WebforjCsrfTokenFilterTest {

  private final WebforjCsrfTokenFilter filter = new WebforjCsrfTokenFilter();

  @Nested
  class Filtering {

    private final MockHttpSession session = new MockHttpSession();

    @Test
    void shouldStoreRequestTokenInSession() throws Exception {
      MockHttpServletRequest request = createRequest("raw-token", "masked-token");
      MockFilterChain chain = new MockFilterChain();

      filter.doFilter(request, new MockHttpServletResponse(), chain);

      CsrfToken stored =
          (CsrfToken) session.getAttribute(WebforjCsrfTokenFilter.SESSION_ATTRIBUTE_NAME);
      assertEquals("X-CSRF-TOKEN", stored.getHeaderName());
      assertEquals("_csrf", stored.getParameterName());
      assertEquals("masked-token", stored.getToken());
      assertEquals(request, chain.getRequest());
    }

    @Test
    void shouldKeepStoredTokenWhileSessionTokenIsUnchanged() throws Exception {
      filter.doFilter(createRequest("raw-token", "masked-one"), new MockHttpServletResponse(),
          new MockFilterChain());

      filter.doFilter(createRequest("raw-token", "masked-two"), new MockHttpServletResponse(),
          new MockFilterChain());

      CsrfToken stored =
          (CsrfToken) session.getAttribute(WebforjCsrfTokenFilter.SESSION_ATTRIBUTE_NAME);
      assertEquals("masked-one", stored.getToken());
    }

    @Test
    void shouldReplaceStoredTokenWhenSessionTokenChanges() throws Exception {
      filter.doFilter(createRequest("raw-one", "masked-one"), new MockHttpServletResponse(),
          new MockFilterChain());

      filter.doFilter(createRequest("raw-two", "masked-two"), new MockHttpServletResponse(),
          new MockFilterChain());

      CsrfToken stored =
          (CsrfToken) session.getAttribute(WebforjCsrfTokenFilter.SESSION_ATTRIBUTE_NAME);
      assertEquals("masked-two", stored.getToken());
    }

    @Test
    void shouldLeaveSessionUntouchedWithoutRequestToken() throws Exception {
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/");
      MockFilterChain chain = new MockFilterChain();

      filter.doFilter(request, new MockHttpServletResponse(), chain);

      assertNull(request.getSession(false));
      assertEquals(request, chain.getRequest());
    }

    private MockHttpServletRequest createRequest(String source, String masked) {
      DeferredCsrfToken deferred = mock(DeferredCsrfToken.class);
      when(deferred.get()).thenReturn(new DefaultCsrfToken("X-CSRF-TOKEN", "_csrf", source));
      MockHttpServletRequest request = new MockHttpServletRequest("GET", "/");
      request.setSession(session);
      request.setAttribute(DeferredCsrfToken.class.getName(), deferred);
      request.setAttribute(CsrfToken.class.getName(),
          new DefaultCsrfToken("X-CSRF-TOKEN", "_csrf", masked));
      return request;
    }
  }

  @Nested
  class Finding {

    @Mock
    private Environment environment;

    @Mock
    private HttpSession httpSession;

    private MockedStatic<Environment> environmentMock;

    @BeforeEach
    void setUp() {
      environmentMock = mockStatic(Environment.class);
    }

    @AfterEach
    void tearDown() {
      environmentMock.close();
    }

    @Test
    void shouldFindNothingWithoutEnvironment() {
      environmentMock.when(Environment::getCurrent).thenReturn(null);

      assertTrue(WebforjCsrfTokenFilter.findToken().isEmpty());
    }

    @Test
    void shouldFindNothingWithoutSessionAccessor() {
      environmentMock.when(Environment::getCurrent).thenReturn(environment);
      when(environment.getSessionAccessor()).thenReturn(Optional.empty());

      assertTrue(WebforjCsrfTokenFilter.findToken().isEmpty());
    }

    @Test
    void shouldFindNothingWhenSessionHoldsNoToken() {
      environmentMock.when(Environment::getCurrent).thenReturn(environment);
      HttpSession.Accessor accessor = SessionAccessorStub.create(httpSession);
      when(environment.getSessionAccessor()).thenReturn(Optional.of(accessor));

      assertTrue(WebforjCsrfTokenFilter.findToken().isEmpty());
    }

    @Test
    void shouldFindStoredToken() {
      CsrfToken token = new DefaultCsrfToken("X-CSRF-TOKEN", "_csrf", "masked-token");
      environmentMock.when(Environment::getCurrent).thenReturn(environment);
      HttpSession.Accessor accessor = SessionAccessorStub.create(httpSession);
      when(environment.getSessionAccessor()).thenReturn(Optional.of(accessor));
      when(httpSession.getAttribute(WebforjCsrfTokenFilter.SESSION_ATTRIBUTE_NAME))
          .thenReturn(token);

      assertEquals(Optional.of(token), WebforjCsrfTokenFilter.findToken());
    }
  }
}
