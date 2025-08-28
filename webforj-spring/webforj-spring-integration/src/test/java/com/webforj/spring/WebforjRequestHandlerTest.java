package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import org.junit.jupiter.api.Test;
import org.springframework.web.servlet.mvc.Controller;

class WebforjRequestHandlerTest {

  @Test
  void shouldExcludePathsWithWildcardPatterns() {
    Controller mockController = mock(Controller.class);
    WebforjRequestHandler handler =
        new WebforjRequestHandler(Arrays.asList("/api/**", "/static/**"), mockController, null);

    // Should exclude paths matching /api/**
    assertTrue(handler.isPathExcluded("/api/users"));
    assertTrue(handler.isPathExcluded("/api/v1/products"));
    assertTrue(handler.isPathExcluded("/api/"));
    assertTrue(handler.isPathExcluded("/api"));

    // Should exclude paths matching /static/**
    assertTrue(handler.isPathExcluded("/static/css/style.css"));
    assertTrue(handler.isPathExcluded("/static/js/app.js"));
    assertTrue(handler.isPathExcluded("/static/"));

    // Should NOT exclude non-matching paths
    assertFalse(handler.isPathExcluded("/app/page"));
    assertFalse(handler.isPathExcluded("/"));
    assertFalse(handler.isPathExcluded("/login"));
  }

  @Test
  void shouldExcludePathsWithExactPatterns() {
    Controller mockController = mock(Controller.class);
    WebforjRequestHandler handler = new WebforjRequestHandler(
        Arrays.asList("/login", "/logout", "/health"), mockController, null);

    // Should exclude exact matches
    assertTrue(handler.isPathExcluded("/login"));
    assertTrue(handler.isPathExcluded("/logout"));
    assertTrue(handler.isPathExcluded("/health"));

    // Should NOT exclude partial matches
    assertFalse(handler.isPathExcluded("/login/page"));
    assertFalse(handler.isPathExcluded("/healthcare"));
    assertFalse(handler.isPathExcluded("/"));
  }

  @Test
  void shouldExcludePathsWithComplexPatterns() {
    Controller mockController = mock(Controller.class);
    WebforjRequestHandler handler = new WebforjRequestHandler(
        Arrays.asList("/api/*/users", "/files/**/*.pdf", "/admin/**"), mockController, null);

    // Single wildcard
    assertTrue(handler.isPathExcluded("/api/v1/users"));
    assertFalse(handler.isPathExcluded("/api/v1/v2/users"));

    // Double wildcard
    assertTrue(handler.isPathExcluded("/files/doc.pdf"));
    assertTrue(handler.isPathExcluded("/files/path/to/doc.pdf"));
    assertFalse(handler.isPathExcluded("/files/doc.txt"));
  }

  @Test
  void shouldHaveHighPriorityToInterceptRequests() {
    Controller mockController = mock(Controller.class);
    WebforjRequestHandler handler = new WebforjRequestHandler(null, mockController, null);

    // The handler should have high priority (-1) to intercept before Spring MVC
    assertEquals(-1, handler.getOrder());
  }
}
