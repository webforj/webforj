package com.webforj.devtools.craftforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.router.model.RouteParam;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

class RoutePatternParserTest {

  @Nested
  @DisplayName("parse")
  class Parse {

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"/products/list", "/users", "/"})
    @DisplayName("Should return empty list for patterns without parameters")
    void shouldReturnEmptyForPatternsWithoutParams(String pattern) {
      List<RouteParam> params = RoutePatternParser.parse(pattern);
      assertTrue(params.isEmpty());
    }

    @Test
    @DisplayName("Should parse simple required parameter")
    void shouldParseSimpleParameter() {
      List<RouteParam> params = RoutePatternParser.parse("/products/:id");

      assertEquals(1, params.size());
      RouteParam param = params.get(0);
      assertEquals("id", param.getName());
      assertNull(param.getConstraint());
      assertFalse(param.isOptional());
      assertFalse(param.isWildcard());
    }

    @Test
    @DisplayName("Should parse optional parameter")
    void shouldParseOptionalParameter() {
      List<RouteParam> params = RoutePatternParser.parse("/products/:id?");

      assertEquals(1, params.size());
      RouteParam param = params.get(0);
      assertEquals("id", param.getName());
      assertTrue(param.isOptional());
      assertFalse(param.isWildcard());
    }

    @Test
    @DisplayName("Should parse wildcard parameter")
    void shouldParseWildcardParameter() {
      List<RouteParam> params = RoutePatternParser.parse("/files/:path*");

      assertEquals(1, params.size());
      RouteParam param = params.get(0);
      assertEquals("path", param.getName());
      assertFalse(param.isOptional());
      assertTrue(param.isWildcard());
    }

    @Test
    @DisplayName("Should parse parameter with constraint")
    void shouldParseParameterWithConstraint() {
      List<RouteParam> params = RoutePatternParser.parse("/products/:id<\\d+>");

      assertEquals(1, params.size());
      RouteParam param = params.get(0);
      assertEquals("id", param.getName());
      assertEquals("\\d+", param.getConstraint());
      assertFalse(param.isOptional());
      assertFalse(param.isWildcard());
    }

    @Test
    @DisplayName("Should parse optional parameter with constraint")
    void shouldParseOptionalWithConstraint() {
      List<RouteParam> params = RoutePatternParser.parse("/products/:id?<\\d+>");

      assertEquals(1, params.size());
      RouteParam param = params.get(0);
      assertEquals("id", param.getName());
      assertEquals("\\d+", param.getConstraint());
      assertTrue(param.isOptional());
    }

    @Test
    @DisplayName("Should parse multiple parameters")
    void shouldParseMultipleParameters() {
      List<RouteParam> params = RoutePatternParser.parse("/products/:category/:id?/:path*");

      assertEquals(3, params.size());

      assertEquals("category", params.get(0).getName());
      assertFalse(params.get(0).isOptional());
      assertFalse(params.get(0).isWildcard());

      assertEquals("id", params.get(1).getName());
      assertTrue(params.get(1).isOptional());
      assertFalse(params.get(1).isWildcard());

      assertEquals("path", params.get(2).getName());
      assertFalse(params.get(2).isOptional());
      assertTrue(params.get(2).isWildcard());
    }

    @Test
    @DisplayName("Should parse standalone wildcard")
    void shouldParseStandaloneWildcard() {
      List<RouteParam> params = RoutePatternParser.parse("/files/*");

      assertEquals(1, params.size());
      RouteParam param = params.get(0);
      assertEquals("*", param.getName());
      assertTrue(param.isWildcard());
    }

    @Test
    @DisplayName("Should skip layout prefix")
    void shouldSkipLayoutPrefix() {
      List<RouteParam> params = RoutePatternParser.parse("@main-layout/:id");

      assertEquals(1, params.size());
      assertEquals("id", params.get(0).getName());
    }

    @Test
    @DisplayName("Should parse parameter with underscore")
    void shouldParseParameterWithUnderscore() {
      List<RouteParam> params = RoutePatternParser.parse("/users/:user_id");

      assertEquals(1, params.size());
      assertEquals("user_id", params.get(0).getName());
    }
  }
}
