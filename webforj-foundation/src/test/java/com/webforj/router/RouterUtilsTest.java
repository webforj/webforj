package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class RouterUtilsTest {

  @ParameterizedTest
  // @formatter:off
  @CsvSource({
      "'', ''", // empty string
      "'', ' '", // blank string
      "'/', '/'", // root path
      "'/example', 'example'", // without starting slash
      "'/example', '/example'", // with starting slash
      "'/example/test', 'example/test'", // without starting slash and subpath
      "'/example/test', '/example/test'", // with starting slash and subpath
      "'/test/', '/test/'", // with trailing slash
      "'/test/', '///test//'", // with redundant slashes
      "'/product/:identifier/:category?/resource/:id<[0-9]*>/:path*', 'product/:identifier///:category?/resource//:id<[0-9]*>////:path*'" // complex pattern path
  })
  // @formatter:on
  void shouldNormalizePath(String expected, String input) {
    String result = RouterUtils.normalizePath(input);
    assertEquals(expected, result);
  }
}
