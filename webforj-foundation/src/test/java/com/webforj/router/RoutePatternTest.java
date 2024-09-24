package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class RoutePatternTest {

  @Nested
  class Matching {

    @Test
    void shouldMatchAndExtractParameters() {
      RoutePattern pattern = new RoutePattern("/customer/:id<[0-9]+>/named/:name/*");
      String path = "/customer/123/named/john/doe";

      assertTrue(pattern.matches(path));

      Map<String, String> params = pattern.getParameters(path);
      assertEquals(3, params.size());
      assertEquals("123", params.get("id"));
      assertEquals("john", params.get("name"));
      assertEquals("doe", params.get("*"));
    }

    @Test
    void shouldNotMatchIncorrectParameters() {
      RoutePattern pattern = new RoutePattern("/customer/:id<[0-9]+>/named/:name/*");
      String path = "/customer/abc/named/john/doe";

      assertFalse(pattern.matches(path));

      Map<String, String> params = pattern.getParameters(path);
      assertTrue(params.isEmpty());
    }

    @Test
    void shouldHandleOptionalParameters() {
      RoutePattern pattern = new RoutePattern("/customer/:id?<[0-9]+>/named/:name?");
      String pathWithAllParams = "/customer/123/named/john";
      String pathWithSomeParams = "/customer/123/named";
      String pathWithNoParams = "/customer/named";

      assertTrue(pattern.matches(pathWithAllParams));
      assertTrue(pattern.matches(pathWithSomeParams));
      assertTrue(pattern.matches(pathWithNoParams));

      Map<String, String> paramsAll = pattern.getParameters(pathWithAllParams);
      assertEquals(2, paramsAll.size());
      assertEquals("123", paramsAll.get("id"));
      assertEquals("john", paramsAll.get("name"));

      Map<String, String> paramsSome = pattern.getParameters(pathWithSomeParams);
      assertEquals(2, paramsSome.size());

      assertEquals("123", paramsSome.get("id"));
      assertNull(paramsSome.get("name"));

      Map<String, String> paramsNone = pattern.getParameters(pathWithNoParams);
      assertEquals(2, paramsAll.size());
      assertNull(paramsNone.get("id"));
      assertNull(paramsNone.get("name"));
    }

    @Test
    void shouldHandleWildcardParameters() {
      RoutePattern pattern = new RoutePattern("/files/*");
      String path = "/files/documents/images/photo.jpg";

      assertTrue(pattern.matches(path));

      Map<String, String> params = pattern.getParameters(path);
      assertEquals(1, params.size());
      assertEquals("documents/images/photo.jpg", params.get("*"));
    }

    @Test
    void shouldNotMatchIncorrectParameterFormat() {
      RoutePattern pattern = new RoutePattern("/customer/:id<[0-9]+>");
      String path = "/customer/abc";

      assertFalse(pattern.matches(path));
    }

    @Test
    void shouldHandleComplexPatternWithOptionalAndWildcard() {
      RoutePattern pattern =
          new RoutePattern("/product/:identifier/:category?/resource/:id<[0-9]*>/:path*");
      String path = "/product/abc/resource/456/docs";

      assertTrue(pattern.matches(path));

      Map<String, String> params = pattern.getParameters(path);

      assertEquals(4, params.size());
      assertEquals("abc", params.get("identifier"));
      assertNull(params.get("category"));
      assertEquals("456", params.get("id"));
      assertEquals("docs", params.get("path"));
    }

    @Test
    void shouldHandleSpecialCharactersInPath() {
      RoutePattern pattern = new RoutePattern("/files/:filename");
      String path = "/files/some%20file.txt";
      assertTrue(pattern.matches(path));

      Map<String, String> params = pattern.getParameters(path);
      assertEquals("some%20file.txt", params.get("filename"));
    }

    @Test
    void shouldMatchStaticPathWithoutParameters() {
      RoutePattern pattern = new RoutePattern("/static/path/to/resource");

      String path = "/static/path/to/resource";
      assertTrue(pattern.matches(path));
      assertTrue(pattern.getParameters(path).isEmpty());

      String nonMatchingPath = "/static/path/to/another/resource";
      assertFalse(pattern.matches(nonMatchingPath));
      assertTrue(pattern.getParameters(nonMatchingPath).isEmpty());
    }

    @Test
    void shouldHandlePatternWithOnlyParameters() {
      RoutePattern pattern = new RoutePattern("/:param1/:param2");
      String path = "/value1/value2";

      assertTrue(pattern.matches(path));
      Map<String, String> params = pattern.getParameters(path);
      assertEquals(2, params.size());
      assertEquals("value1", params.get("param1"));
      assertEquals("value2", params.get("param2"));
    }

    @Test
    void shouldHandleMultipleWildcards() {
      RoutePattern pattern = new RoutePattern("/files/*/*");
      String path = "/files/documents/images/photos";

      assertTrue(pattern.matches(path));
      Map<String, String> params = pattern.getParameters(path);
      assertEquals(1, params.size());
      assertEquals("documents/images/photos", params.get("*"));
    }

    @Test
    void shouldNotMatchInvalidUnescapedCharacters() {
      RoutePattern pattern = new RoutePattern("/files/:filename<[^/]+>");
      String path = "/files/file/with/slash.txt";

      assertFalse(pattern.matches(path));
    }

    @Test
    void shouldMatchAndExtractParametersWithLeadingSlash() {
      RoutePattern pattern = new RoutePattern("/customer/:id<[0-9]+>/");
      String path = "/customer/123/";

      assertTrue(pattern.matches(path));

      Map<String, String> params = pattern.getParameters(path);
      assertEquals(1, params.size());
      assertEquals("123", params.get("id"));
    }

    @Test
    void shouldMatchAndExtractParametersWithoutLeadingSlash() {
      RoutePattern pattern = new RoutePattern("files/:id");
      String path = "files/Dashboard";

      assertTrue(pattern.matches(path));

      Map<String, String> params = pattern.getParameters(path);

      assertEquals(1, params.size());
      assertEquals("Dashboard", params.get("id"));
    }

    @Test
    void shouldNotMatchInvalidRegexInPattern() {
      assertThrows(IllegalArgumentException.class, () -> new RoutePattern("/customer/:id<[^>*>"));
    }
  }

  @Nested
  class UrlGenerating {

    @Test
    void shouldBuildUrlWithParameters() {
      RoutePattern pattern = new RoutePattern("/customer/:id<[0-9]+>/named/:name/*");

      Map<String, String> params = new HashMap<>();
      params.put("id", "123");
      params.put("name", "john");
      params.put("*", "extra/path");

      String url = pattern.generateUrl(params);
      assertEquals("/customer/123/named/john/extra/path", url);
    }

    @Test
    void shouldBuildUrlWithMissingOptionalParameters() {
      RoutePattern pattern = new RoutePattern("/customer/:id?<[0-9]+>/named/:name?");
      Map<String, String> params = new HashMap<>();
      params.put("id", "123");

      String url = pattern.generateUrl(params);
      assertEquals("/customer/123/named", url);
    }

    @Test
    void shouldThrowExceptionForMissingRequiredParameters() {
      RoutePattern pattern = new RoutePattern("/customer/:id<[0-9]+>/named/:name/*");
      Map<String, String> params = new HashMap<>();
      params.put("name", "john");

      IllegalArgumentException t =
          assertThrows(IllegalArgumentException.class, () -> pattern.generateUrl(params));
      assertEquals("Missing required parameter: id", t.getMessage());
    }

    @Test
    void shouldHandleComplexPatternWithOptionalAndWildcardForUrl() {
      RoutePattern pattern =
          new RoutePattern("/product/:identifier/:category?/resource/:id<[0-9]*>/:path*");

      Map<String, String> emptyWildcardPath = new HashMap<>();
      emptyWildcardPath.put("identifier", "abc");
      emptyWildcardPath.put("id", "789");
      emptyWildcardPath.put("path*", "pdf/foo/bar");

      String urlWithEmptyWildcard = pattern.generateUrl(emptyWildcardPath);
      assertEquals("/product/abc/resource/789/pdf/foo/bar", urlWithEmptyWildcard);

      Map<String, String> onlyRequiredParams = new HashMap<>();
      onlyRequiredParams.put("identifier", "xyz");
      onlyRequiredParams.put("id", "");

      String urlOnlyRequired = pattern.generateUrl(onlyRequiredParams);
      assertEquals("/product/xyz/resource", urlOnlyRequired);
    }
  }

  @Nested
  class LayoutHandling {

    @Test
    void shouldIgnoreLayoutsWhenMatching() {
      RoutePattern pattern = new RoutePattern("/@dashboard/blog/@container/:id<[0-9]+>");
      String path = "/blog/123";

      assertTrue(pattern.matches(path));

      Map<String, String> params = pattern.getParameters(path);
      assertEquals(1, params.size());
      assertEquals("123", params.get("id"));
    }

    @ParameterizedTest
    @CsvSource({
    // @formatter:off
        // Pattern, Path, Matches
        "'/', '/', true",
        "'/', '', true",
        "'@layout/', '/', true",
        "'@layout/', '', true",
        "'/@layout/', '/', true",
        "'/@layout/', '', true",
        "'@layout/@another/', '/', true",
        "'@layout/@another/', '' , true",
        "'/@layout/@another/', '/', true",
        "'/@layout/@another/', '' , true",
        "'@layout/@another', '/', true",
        "'@layout/@another', '', true",
        "'/@layout/@another', '/', true",
        "'/@layout/@another', '', true",
        "'@layout/@another/other', '/other', true",
        "'/@layout/@another/other', '/other', true",
    // @formatter:on
    })
    void shouldMatchPathsAfterStrippingLayoutSegments(String pattern, String path,
        boolean matches) {
      RoutePattern routePattern = new RoutePattern(pattern);
      assertEquals(matches, routePattern.matches(path),
          "Expected pattern '" + pattern + "' to match path '" + path + "'");
    }

    @Test
    void shouldIgnoreLayoutsWhenGeneratingUrl() {
      RoutePattern pattern = new RoutePattern("/@dashboard/blog/@container/:id<[0-9]+>");
      Map<String, String> params = new HashMap<>();
      params.put("id", "123");

      String url = pattern.generateUrl(params);
      assertEquals("/blog/123", url);
    }
  }
}
