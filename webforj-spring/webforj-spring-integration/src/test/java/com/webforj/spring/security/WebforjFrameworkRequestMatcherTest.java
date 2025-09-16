package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.webforj.spring.SpringConfigurationProperties;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class WebforjFrameworkRequestMatcherTest {

  private WebforjFrameworkRequestMatcher matcher;

  @Mock
  private HttpServletRequest request;

  @Mock
  private SpringConfigurationProperties properties;

  @Nested
  class ServletMappingHandling {

    @Test
    void shouldHandleRootMapping() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);

      when(request.getServletPath()).thenReturn("/webforjServlet/webapprmi");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }

    @Test
    void shouldHandlePrefixedMapping() {
      when(properties.getServletMapping()).thenReturn("/myapp/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);

      when(request.getServletPath()).thenReturn("/myapp/webapprmi");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }
  }

  @Nested
  class RpcEndpointMatching {

    @BeforeEach
    void setup() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);
    }

    @Test
    void shouldMatchWebAppRmiEndpoint() {
      when(request.getServletPath()).thenReturn("/webapprmi");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }

    @Test
    void shouldMatchWebAppRmiWithSubPath() {
      when(request.getServletPath()).thenReturn("/webapprmi");
      when(request.getPathInfo()).thenReturn("/something");

      assertTrue(matcher.matches(request));
    }

    @Test
    void shouldNotMatchNonRpcPath() {
      when(request.getServletPath()).thenReturn("/api");
      when(request.getPathInfo()).thenReturn("/users");

      assertFalse(matcher.matches(request));
    }
  }

  @Nested
  class LibraryResources {

    @BeforeEach
    void setup() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);
    }

    @Test
    void shouldMatchWebappLibPath() {
      when(request.getServletPath()).thenReturn("/webapp/_lib");
      when(request.getPathInfo()).thenReturn("/script.js");

      assertTrue(matcher.matches(request));
    }

    @Test
    void shouldMatchWebappLibWithFiles() {
      when(request.getServletPath()).thenReturn("/webapp/_lib/dwc-ui.min.js");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }
  }

  @Nested
  class IconsDirectory {

    @BeforeEach
    void setup() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);
    }

    @Test
    void shouldMatchDefaultIconsPath() {
      when(properties.getIconsDir()).thenReturn(null);
      when(request.getServletPath()).thenReturn("/icons/logo.png");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }

    @Test
    void shouldMatchCustomIconsPath() {
      when(properties.getIconsDir()).thenReturn("custom-icons");
      when(request.getServletPath()).thenReturn("/custom-icons/icon.svg");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }

    @Test
    void shouldNormalizeIconsPath() {
      when(properties.getIconsDir()).thenReturn("images");
      when(request.getServletPath()).thenReturn("/images/pic.jpg");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }
  }

  @Nested
  class AssetsDirectory {

    @BeforeEach
    void setup() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);
    }

    @Test
    void shouldMatchDefaultAssetsPath() {
      when(properties.getAssetsDir()).thenReturn(null);
      when(request.getServletPath()).thenReturn("/static/style.css");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }

    @Test
    void shouldMatchCustomAssetsPath() {
      when(properties.getAssetsDir()).thenReturn("assets");
      when(request.getServletPath()).thenReturn("/assets/app.js");
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }
  }
}
