package com.webforj.spring.devtools.browser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import java.net.InetAddress;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.core.env.Environment;

class BrowserLauncherTest {

  private BrowserLauncher browserLauncher;

  @Mock
  private Environment mockEnvironment;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    browserLauncher = new BrowserLauncher(mockEnvironment);
  }


  @Test
  void shouldBuildUrlWithDefaultLocalhostHost() {
    when(mockEnvironment.getProperty("server.port", Integer.class, 8080)).thenReturn(8080);
    when(mockEnvironment.getProperty("webforj.servlet-mapping", "/*")).thenReturn("/*");
    when(mockEnvironment.getProperty("server.ssl.enabled", Boolean.class, false)).thenReturn(false);
    when(mockEnvironment.getProperty("webforj.devtools.browser.host", "localhost"))
        .thenReturn("localhost");

    String url = browserLauncher.buildUrl();

    assertEquals("http://localhost:8080", url);
  }

  @Test
  void shouldBuildUrlWithHostnameHost() throws Exception {
    when(mockEnvironment.getProperty("server.port", Integer.class, 8080)).thenReturn(8080);
    when(mockEnvironment.getProperty("webforj.servlet-mapping", "/*")).thenReturn("/*");
    when(mockEnvironment.getProperty("server.ssl.enabled", Boolean.class, false)).thenReturn(false);
    when(mockEnvironment.getProperty("webforj.devtools.browser.host", "localhost"))
        .thenReturn("hostname");

    String url = browserLauncher.buildUrl();
    String expectedHostname = InetAddress.getLocalHost().getHostName();

    assertEquals("http://" + expectedHostname + ":8080", url);
  }

  @Test
  void shouldBuildUrlWithIpAddressHost() throws Exception {
    when(mockEnvironment.getProperty("server.port", Integer.class, 8080)).thenReturn(8080);
    when(mockEnvironment.getProperty("webforj.servlet-mapping", "/*")).thenReturn("/*");
    when(mockEnvironment.getProperty("server.ssl.enabled", Boolean.class, false)).thenReturn(false);
    when(mockEnvironment.getProperty("webforj.devtools.browser.host", "localhost"))
        .thenReturn("ip-address");

    String url = browserLauncher.buildUrl();
    String expectedIpAddress = InetAddress.getLocalHost().getHostAddress();

    assertEquals("http://" + expectedIpAddress + ":8080", url);
  }

  @Test
  void shouldBuildUrlWithAllCustomSettings() {
    when(mockEnvironment.getProperty("server.port", Integer.class, 8080)).thenReturn(9090);
    when(mockEnvironment.getProperty("webforj.servlet-mapping", "/*")).thenReturn("/revamped/*");
    when(mockEnvironment.getProperty("server.ssl.enabled", Boolean.class, false)).thenReturn(true);
    when(mockEnvironment.getProperty("webforj.devtools.browser.host", "localhost"))
        .thenReturn("localhost");

    String url = browserLauncher.buildUrl();

    assertEquals("https://localhost:9090/revamped", url);
  }
}
