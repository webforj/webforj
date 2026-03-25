package com.webforj.component.webswing;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class WebswingOptionsTest {

  private WebswingOptions options;

  @BeforeEach
  void setUp() {
    options = new WebswingOptions();
  }

  @Test
  void shouldSetAndGetAutoStart() {
    WebswingOptions result = options.setAutoStart(true);

    assertTrue(options.isAutoStart());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetSecurityToken() {
    String token = "secure-token-123";
    WebswingOptions result = options.setSecurityToken(token);

    assertEquals(token, options.getSecurityToken());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetRealm() {
    String realm = "secure-realm-123";
    WebswingOptions result = options.setRealm(realm);

    assertEquals(realm, options.getRealm());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetArgs() {
    String args = "--debug --verbose";
    WebswingOptions result = options.setArgs(args);

    assertEquals(args, options.getArgs());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetRecording() {
    WebswingOptions result = options.setRecording(true);

    assertTrue(options.isRecording());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetConnectionUrl() {
    String url = "ws://localhost:8080/webswing";
    WebswingOptions result = options.setConnectionUrl(url);

    assertEquals(url, options.getConnectionUrl());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetDebugPort() {
    Integer port = 9999;
    WebswingOptions result = options.setDebugPort(port);

    assertEquals(port, options.getDebugPort());
    assertSame(options, result);
  }
}
