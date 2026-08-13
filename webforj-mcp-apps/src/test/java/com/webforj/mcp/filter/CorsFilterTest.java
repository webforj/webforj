package com.webforj.mcp.filter;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class CorsFilterTest {

  private final CorsFilter filter = new CorsFilter(List.of());
  private final HttpServletRequest request = mock(HttpServletRequest.class);
  private final HttpServletResponse response = mock(HttpServletResponse.class);
  private final FilterChain chain = mock(FilterChain.class);

  @Test
  @DisplayName("Should leave a request without an origin untouched")
  void shouldLeaveSameOriginUntouched() throws Exception {
    when(request.getHeader("Origin")).thenReturn(null);

    filter.doFilter(request, response, chain);

    verify(response, never()).setHeader(anyString(), anyString());
    verify(chain).doFilter(request, response);
  }

  @ParameterizedTest
  @ValueSource(strings = {"https://a1b2c3.claudemcpcontent.com",
      "https://a1b2c3.claudeusercontent.com", "https://web-sandbox.oaiusercontent.com",
      "codex-sandbox://mcp-server-webforj-da34d426d5b025fa.web-sandbox.oaiusercontent.com"})
  @DisplayName("Should answer every known host sandbox origin out of the box")
  void shouldAnswerKnownHostSandboxOrigins(String origin) throws Exception {
    when(request.getHeader("Origin")).thenReturn(origin);
    when(request.getMethod()).thenReturn("GET");

    filter.doFilter(request, response, chain);

    verify(response).setHeader("Access-Control-Allow-Origin", origin);
    verify(response).setHeader("Access-Control-Allow-Credentials", "true");
    verify(response).setHeader("Vary", "Origin");
    verify(chain).doFilter(request, response);
  }

  @Test
  @DisplayName("Should answer the preflight of a known host without running the chain")
  void shouldAnswerKnownHostPreflight() throws Exception {
    when(request.getHeader("Origin")).thenReturn("https://web-sandbox.oaiusercontent.com");
    when(request.getMethod()).thenReturn("OPTIONS");
    when(request.getHeader("Access-Control-Request-Headers")).thenReturn("content-type, accept");

    filter.doFilter(request, response, chain);

    verify(response).setHeader("Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS");
    verify(response).setHeader("Access-Control-Allow-Headers", "content-type, accept");
    verify(response).setStatus(HttpServletResponse.SC_NO_CONTENT);
    verify(chain, never()).doFilter(request, response);
  }

  @Test
  @DisplayName("Should let the host read the session header the endpoint answers with")
  void shouldExposeSessionHeader() throws Exception {
    when(request.getHeader("Origin")).thenReturn("https://a1b2c3.claudeusercontent.com");
    when(request.getMethod()).thenReturn("POST");

    filter.doFilter(request, response, chain);

    verify(response).setHeader("Access-Control-Expose-Headers", "Mcp-Session-Id");
  }

  @Test
  @DisplayName("Should answer the component library for every origin without credentials")
  void shouldAnswerComponentLibraryForEveryOrigin() throws Exception {
    when(request.getHeader("Origin")).thenReturn("http://localhost:8080");
    when(request.getMethod()).thenReturn("GET");
    when(request.getRequestURI())
        .thenReturn("/webforjServlet/webapp/_lib/components/dwc-ui.esm.js");

    filter.doFilter(request, response, chain);

    verify(response).setHeader("Access-Control-Allow-Origin", "*");
    verify(response, never()).setHeader(eq("Access-Control-Allow-Credentials"), anyString());
    verify(chain).doFilter(request, response);
  }

  @Test
  @DisplayName("Should answer the component library preflight without running the chain")
  void shouldAnswerComponentLibraryPreflight() throws Exception {
    when(request.getHeader("Origin")).thenReturn("http://localhost:8080");
    when(request.getMethod()).thenReturn("OPTIONS");
    when(request.getRequestURI())
        .thenReturn("/webforjServlet/webapp/_lib/components/dwc-ui.esm.js");

    filter.doFilter(request, response, chain);

    verify(response).setHeader("Access-Control-Allow-Origin", "*");
    verify(response).setHeader("Access-Control-Allow-Methods", "GET, OPTIONS");
    verify(response).setStatus(HttpServletResponse.SC_NO_CONTENT);
    verify(chain, never()).doFilter(request, response);
  }

  @Test
  @DisplayName("Should refuse an unknown origin without configuration")
  void shouldRefuseUnknownOriginByDefault() throws Exception {
    when(request.getHeader("Origin")).thenReturn("https://host.example.com");
    when(request.getMethod()).thenReturn("GET");

    filter.doFilter(request, response, chain);

    verify(response, never()).setHeader(anyString(), anyString());
    verify(chain).doFilter(request, response);
  }

  @Test
  @DisplayName("Should refuse a lookalike of a host sandbox domain")
  void shouldRefuseSandboxLookalike() throws Exception {
    when(request.getHeader("Origin")).thenReturn("https://evil-claudemcpcontent.com");
    when(request.getMethod()).thenReturn("GET");

    filter.doFilter(request, response, chain);

    verify(response, never()).setHeader(anyString(), anyString());
    verify(chain).doFilter(request, response);
  }

  @Test
  @DisplayName("Should pass a request that is not http straight through")
  void shouldPassNonHttpThrough() throws Exception {
    ServletRequest plainRequest = mock(ServletRequest.class);
    ServletResponse plainResponse = mock(ServletResponse.class);

    filter.doFilter(plainRequest, plainResponse, chain);

    verify(chain).doFilter(plainRequest, plainResponse);
  }

  @Test
  @DisplayName("Should answer an origin the configuration adds beside the hosts")
  void shouldAnswerConfiguredOrigin() throws Exception {
    CorsFilter extended = new CorsFilter(List.of("https://host.example.com"));
    when(request.getHeader("Origin")).thenReturn("https://host.example.com");
    when(request.getMethod()).thenReturn("GET");

    extended.doFilter(request, response, chain);

    verify(response).setHeader("Access-Control-Allow-Origin", "https://host.example.com");
    verify(chain).doFilter(request, response);
  }

  @Test
  @DisplayName("Should answer an origin matching a configured pattern")
  void shouldAnswerConfiguredPattern() throws Exception {
    CorsFilter extended = new CorsFilter(List.of("https://*.tunnel.example"));
    when(request.getHeader("Origin")).thenReturn("https://blue-fox.tunnel.example");
    when(request.getMethod()).thenReturn("GET");

    extended.doFilter(request, response, chain);

    verify(response).setHeader("Access-Control-Allow-Origin", "https://blue-fox.tunnel.example");
    verify(chain).doFilter(request, response);
  }

  @Test
  @DisplayName("Should refuse the preflight of an origin outside every list")
  void shouldRefuseDisallowedPreflight() throws Exception {
    CorsFilter extended = new CorsFilter(List.of("https://host.example.com"));
    when(request.getHeader("Origin")).thenReturn("https://stranger.example.com");
    when(request.getMethod()).thenReturn("OPTIONS");

    extended.doFilter(request, response, chain);

    verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
    verify(chain, never()).doFilter(request, response);
  }
}
