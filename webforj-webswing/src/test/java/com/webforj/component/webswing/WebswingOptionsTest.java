package com.webforj.component.webswing;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
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
  void shouldSetAndGetAutoReconnect() {
    Integer reconnectTime = 5000;
    WebswingOptions result = options.setAutoReconnect(reconnectTime);

    assertEquals(reconnectTime, options.getAutoReconnect());
    assertSame(options, result);
  }

  void shouldSetAndGetSecurityToken() {
    String token = "secure-token-123";
    WebswingOptions result = options.setSecurityToken(token);

    assertEquals(token, options.getSecurityToken());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetDisableLogout() {
    WebswingOptions result = options.setDisableLogout(true);

    assertTrue(options.isDisableLogout());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetDisableLogin() {
    WebswingOptions result = options.setDisableLogin(true);

    assertTrue(options.isDisableLogin());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetSyncClipboard() {
    WebswingOptions result = options.setSyncClipboard(true);

    assertTrue(options.isSyncClipboard());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetSecurityTokenProvider() {
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

  @Test
  void shouldSetAndGetJavaCallTimeout() {
    int timeout = 5000;
    WebswingOptions result = options.setJavaCallTimeout(timeout);

    assertEquals(timeout, options.getJavaCallTimeout());
    assertSame(options, result);
  }

  @Test
  void shouldSetAndGetPingParams() {
    WebswingOptions.PingParams pingParams = new WebswingOptions.PingParams();
    pingParams.setCount(5);
    pingParams.setInterval(2000);
    pingParams.setMaxLatency(1500);
    pingParams.setNotifyIf(3);
    pingParams.setUrl("http://ping.example.com");

    WebswingOptions result = options.setPingParams(pingParams);

    assertEquals(pingParams, options.getPingParams());
    assertSame(options, result);
  }

  @Nested
  class PingParamsTest {

    private WebswingOptions.PingParams pingParams;

    @BeforeEach
    void setUp() {
      pingParams = new WebswingOptions.PingParams();
    }

    @Test
    void shouldSetAndGetCount() {
      int count = 10;
      WebswingOptions.PingParams result = pingParams.setCount(count);

      assertEquals(count, pingParams.getCount());
      assertSame(pingParams, result);
    }

    @Test
    void shouldSetAndGetInterval() {
      int interval = 10;
      WebswingOptions.PingParams result = pingParams.setInterval(interval);

      assertEquals(interval, pingParams.getInterval());
      assertSame(pingParams, result);
    }

    @Test
    void shouldSetAndGetMaxLatency() {
      int maxLatency = 1000;
      WebswingOptions.PingParams result = pingParams.setMaxLatency(maxLatency);

      assertEquals(maxLatency, pingParams.getMaxLatency());
      assertSame(pingParams, result);
    }

    @Test
    void shouldSetAndGetNotifyIf() {
      int notifyIf = 5;
      WebswingOptions.PingParams result = pingParams.setNotifyIf(notifyIf);

      assertEquals(notifyIf, pingParams.getNotifyIf());
      assertSame(pingParams, result);
    }

    @Test
    void shouldSetAndGetUrl() {
      String url = "http://ping.example.com";
      WebswingOptions.PingParams result = pingParams.setUrl(url);

      assertEquals(url, pingParams.getUrl());
      assertSame(pingParams, result);
    }
  }
}

