package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjConfig;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.BBjThinClient;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.util.List;
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
  BBjWebManager webManager;
  BBjConfig config;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    thinClient = mock(BBjThinClient.class);
    sysGui = mock(BBjSysGui.class);
    webManager = mock(BBjWebManager.class);
    config = mock(BBjConfig.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(api.getThinClient()).thenReturn(thinClient);
    when(api.getWebManager()).thenReturn(webManager);
    when(api.getConfig()).thenReturn(config);
    when(environment.getSysGui()).thenReturn(sysGui);

    request = spy(Request.class);
    when(request.getEnvironment()).thenReturn(environment);
  }

  @Test
  void shouldGetUrl() throws BBjException {
    String url = "http://localhost:8080";
    when(webManager.getUrl()).thenReturn(url);
    assertEquals(url, request.getUrl());
  }

  @Test
  void shouldGetProtocol() throws BBjException {
    String url = "http://localhost:8080";
    when(webManager.getUrl()).thenReturn(url);
    assertEquals("http", request.getProtocol());
  }

  @Test
  void shouldGetHost() throws BBjException {
    String url = "http://localhost:8080";
    when(webManager.getUrl()).thenReturn(url);
    assertEquals("localhost", request.getHost());
  }

  @Test
  void shouldGetPort() throws BBjException {
    String url = "http://localhost:8080";
    when(webManager.getUrl()).thenReturn(url);
    assertEquals("8080", request.getPort());
  }

  @Test
  void shouldGetQueryParameter() throws BBjException {
    when(config.clientEnv("key", true)).thenReturn("value");
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
  void shouldGetLocaleWithUnderscores() throws BBjException {
    when(thinClient.getClientLocale()).thenReturn("de_DE");
    assertEquals(Locale.forLanguageTag("de-DE"), request.getLocale());
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
  void shouldGetPreferredLocalesWithUnderscores() throws BBjException {
    BBjVector locales = new BBjVector();
    locales.add("de_DE");
    locales.add("en");
    locales.add("en_US");
    locales.add("ar");
    locales.add("de");
    when(thinClient.getClientLocales()).thenReturn(locales);

    List<Locale> result = request.getPreferredLocales();
    assertEquals(5, result.size());
    assertEquals(Locale.forLanguageTag("de-DE"), result.get(0));
    assertEquals(Locale.forLanguageTag("en"), result.get(1));
    assertEquals(Locale.forLanguageTag("en-US"), result.get(2));
    assertEquals(Locale.forLanguageTag("ar"), result.get(3));
    assertEquals(Locale.forLanguageTag("de"), result.get(4));
  }

  @Test
  void shouldFilterEmptyPreferredLocales() throws BBjException {
    BBjVector locales = new BBjVector();
    locales.add("");
    locales.add("en");
    locales.add("  ");
    when(thinClient.getClientLocales()).thenReturn(locales);

    List<Locale> result = request.getPreferredLocales();
    assertEquals(1, result.size());
    assertEquals(Locale.forLanguageTag("en"), result.get(0));
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
