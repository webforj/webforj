package com.webforj.router;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class RouteFunctionalityTest {

  @Test
  void shouldCompileAndMatchSimpleRoute() {
    RoutePattern pattern = new RoutePattern("user/profile");

    assertTrue(pattern.matches("user/profile"));
    assertFalse(pattern.matches("user/profile/edit"));
  }

  @Test
  void shouldCompileAndComposeRouteWithRequiredParameter() {
    RoutePattern pattern = new RoutePattern("user/:userId");

    Map<String, String> parameters = new HashMap<>();
    parameters.put("userId", "123");

    assertEquals("user/123", pattern.composePath(parameters));

    assertTrue(pattern.matches("user/123"));
    assertFalse(pattern.matches("user"));
    assertFalse(pattern.matches("user/abc"));
  }

  @Test
  void shouldCompileAndComposeRouteWithOptionalParameter() {
    RoutePattern pattern = new RoutePattern("user/:userId?/edit");

    Map<String, String> parameters = new HashMap<>();
    parameters.put("userId", "123");
    assertEquals("user/123/edit", pattern.composePath(parameters));

    parameters.clear();
    assertEquals("user/edit", pattern.composePath(parameters));

    assertTrue(pattern.matches("user/123/edit"));
    assertTrue(pattern.matches("user/edit"));
    assertFalse(pattern.matches("user/123"));
  }

  @Test
  void shouldCompileAndComposeRouteWithWildcardParameter() {
    RoutePattern pattern = new RoutePattern("files/:path*");

    Map<String, String> parameters = new HashMap<>();
    parameters.put("path", "docs/images");
    assertEquals("files/docs/images", pattern.composePath(parameters));

    parameters.put("path", "docs");
    assertEquals("files/docs", pattern.composePath(parameters));

    parameters.put("path", "");
    assertEquals("files", pattern.composePath(parameters));

    assertTrue(pattern.matches("files/docs/images"));
    assertTrue(pattern.matches("files/docs"));
    assertTrue(pattern.matches("files"));
    assertFalse(pattern.matches("file/docs"));
  }

  @Test
  void shouldCompileAndComposeRouteWithCustomRegex() {
    RoutePattern pattern = new RoutePattern("product/:productId([0-9]+)");

    Map<String, String> parameters = new HashMap<>();
    parameters.put("productId", "123");
    assertEquals("product/123", pattern.composePath(parameters));

    assertTrue(pattern.matches("product/123"));
    assertFalse(pattern.matches("product/abc"));
  }

  @Test
  void shouldDecomposePathIntoParameters() {
    RoutePattern pattern = new RoutePattern("user/:userId/edit/:section?");

    Map<String, String> extractedParameters =
        pattern.extractParameters("user/123/edit/profile");
    assertEquals(2, extractedParameters.size());
    assertEquals("123", extractedParameters.get("userId"));
    assertEquals("profile", extractedParameters.get("section"));

    extractedParameters = pattern.extractParameters("user/123/edit");
    assertEquals(1, extractedParameters.size());
    assertEquals("123", extractedParameters.get("userId"));
    assertNull(extractedParameters.get("section"));
  }

  @Test
  void shouldHandleComplexRoutes() {
    RoutePattern pattern = new RoutePattern("api/:version?/users/:userId([0-9]+)/details/:detailId?");

    Map<String, String> parameters = new HashMap<>();
    parameters.put("version", "v1");
    parameters.put("userId", "42");
    parameters.put("detailId", "99");
    assertEquals("api/v1/users/42/details/99", pattern.composePath(parameters));

    parameters.remove("version");
    assertEquals("api/users/42/details/99", pattern.composePath(parameters));

    parameters.remove("detailId");
    assertEquals("api/users/42/details", pattern.composePath(parameters));

    assertTrue(pattern.matches("api/v1/users/42/details/99"));
    assertTrue(pattern.matches("api/users/42/details/99"));
    assertTrue(pattern.matches("api/users/42/details"));
    assertFalse(pattern.matches("api/users/details/99"));
  }
}
