package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.webforj.spring.SpringConfigurationProperties;
import jakarta.servlet.http.HttpServletRequest;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
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

    @ParameterizedTest
    @MethodSource("servletMappingTestCases")
    void shouldHandleServletMappings(String servletMapping, String servletPath) {
      when(properties.getServletMapping()).thenReturn(servletMapping);
      matcher = new WebforjFrameworkRequestMatcher(properties);

      when(request.getServletPath()).thenReturn(servletPath);
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }

    static Stream<Arguments> servletMappingTestCases() {
      return Stream.of(Arguments.of("/*", "/webforjServlet/webapprmi"),
          Arguments.of("/myapp/*", "/myapp/webapprmi"));
    }
  }

  @Nested
  class RpcEndpointMatching {

    @BeforeEach
    void setup() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);
    }

    @ParameterizedTest
    @MethodSource("rpcEndpointTestCases")
    void shouldHandleRpcEndpoints(String servletPath, String pathInfo, boolean expectedMatch) {
      when(request.getServletPath()).thenReturn(servletPath);
      when(request.getPathInfo()).thenReturn(pathInfo);

      assertEquals(expectedMatch, matcher.matches(request));
    }

    static Stream<Arguments> rpcEndpointTestCases() {
      return Stream.of(Arguments.of("/webapprmi", null, true),
          Arguments.of("/webapprmi", "/something", true), Arguments.of("/api", "/users", false));
    }
  }

  @Nested
  class LibraryResources {

    @BeforeEach
    void setup() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);
    }

    @ParameterizedTest
    @MethodSource("libraryResourceTestCases")
    void shouldHandleLibraryResources(String servletPath, String pathInfo) {
      when(request.getServletPath()).thenReturn(servletPath);
      when(request.getPathInfo()).thenReturn(pathInfo);

      assertTrue(matcher.matches(request));
    }

    static Stream<Arguments> libraryResourceTestCases() {
      return Stream.of(Arguments.of("/webapp/_lib", "/script.js"),
          Arguments.of("/webapp/_lib/dwc-ui.min.js", null));
    }
  }

  @Nested
  class IconsDirectory {

    @BeforeEach
    void setup() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);
    }

    @ParameterizedTest
    @MethodSource("iconsDirectoryTestCases")
    void shouldHandleIconsDirectory(String iconsDir, String servletPath) {
      when(properties.getIconsDir()).thenReturn(iconsDir);
      when(request.getServletPath()).thenReturn(servletPath);
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }

    static Stream<Arguments> iconsDirectoryTestCases() {
      return Stream.of(Arguments.of(null, "/icons/logo.png"),
          Arguments.of("custom-icons", "/custom-icons/icon.svg"),
          Arguments.of("images", "/images/pic.jpg"));
    }
  }

  @Nested
  class AssetsDirectory {

    @BeforeEach
    void setup() {
      when(properties.getServletMapping()).thenReturn("/*");
      matcher = new WebforjFrameworkRequestMatcher(properties);
    }

    @ParameterizedTest
    @MethodSource("assetsDirectoryTestCases")
    void shouldHandleAssetsDirectory(String assetsDir, String servletPath) {
      when(properties.getAssetsDir()).thenReturn(assetsDir);
      when(request.getServletPath()).thenReturn(servletPath);
      when(request.getPathInfo()).thenReturn(null);

      assertTrue(matcher.matches(request));
    }

    static Stream<Arguments> assetsDirectoryTestCases() {
      return Stream.of(Arguments.of(null, "/static/style.css"),
          Arguments.of("assets", "/assets/app.js"));
    }
  }
}
