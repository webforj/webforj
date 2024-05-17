package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.bridge.WebforjBBjBridge;
import java.util.Locale;
import java.util.TimeZone;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class RequestTest {
  Request request;
  Environment environment;
  BBjAPI api;
  BBjSysGui sysGui;
  BBjThinClient thinClient;
  WebforjBBjBridge bridge;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    thinClient = mock(BBjThinClient.class);
    sysGui = mock(BBjSysGui.class);
    bridge = mock(WebforjBBjBridge.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(api.getThinClient()).thenReturn(thinClient);
    when(environment.getSysGui()).thenReturn(sysGui);
    when(environment.getWeforjHelper()).thenReturn(bridge);

    request = spy(Request.class);
    when(request.getEnvironment()).thenReturn(environment);
  }

  @Test
  void shouldGetQueryParameter() {
    when(bridge.getQueryParam("key")).thenReturn("value");
    assertEquals("value", request.getQueryParameter("key"));
  }

  @Test
  void shouldGetIPAddress() throws BBjException {
    when(thinClient.getClientIPAddress()).thenReturn("ipAddress");
    assertEquals("ipAddress", request.getIPAddress());
  }

  @Test
  void shouldGetPublicIPAddress() throws BBjException {
    when(thinClient.getPublicIPAddress()).thenReturn("publicIPAddress");
    assertEquals("publicIPAddress", request.getPublicIPAddress());
  }

  @Test
  void shouldGetLocale() throws BBjException {
    when(thinClient.getClientLocale()).thenReturn("zh-Hans-CN");
    assertEquals(Locale.forLanguageTag("zh-Hans-CN"), request.getLocale());
  }

  @Test
  void shouldGetPreferredLocales() throws BBjException {
    BBjVector locales = new BBjVector();
    locales.add("zh-Hans-CN");
    locales.add("en-US");
    when(thinClient.getClientLocales()).thenReturn(locales);
    assertEquals(Locale.forLanguageTag("zh-Hans-CN"), request.getPreferredLocales().get(0));
    assertEquals(Locale.forLanguageTag("en-US"), request.getPreferredLocales().get(1));
  }

  @Test
  void shouldGetSystemName() throws BBjException {
    String result = "Mac OS X";
    when(thinClient.getClientOSName()).thenReturn(result);
    assertEquals(result, request.getSystemName());
  }

  @Test
  void shouldGetSystemVersion() throws BBjException {
    String result = "10.15.7";
    when(thinClient.getClientOSVersion()).thenReturn(result);
    assertEquals(result, request.getSystemVersion());
  }

  @Test
  void shouldGetTimeZone() throws BBjException {
    TimeZone result = TimeZone.getTimeZone("Asia/Shanghai");
    when(thinClient.getClientTimeZone()).thenReturn(result);
    assertEquals(result, request.getTimeZone());
  }
}
