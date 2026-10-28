package com.webforj.bbjship.foundation.bbjshipclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.bbjship.foundation.BBjShipClient;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import java.io.IOException;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.URI;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ClassicHttpRequest;
import org.apache.hc.core5.http.protocol.HttpContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Answers;
import org.mockito.MockedStatic;

class BBjShipClientConfigurationTest {

  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret-password";
  private static final String CLIENT_VERSION = "26.03";

  @TempDir
  Path temporary;

  @ParameterizedTest
  @ValueSource(strings = {"file:///webforj-bbjship",
      "http://admin:secret@localhost/webforj-bbjship", "http://localhost/webforj-bbjship#fragment",
      "http://localhost/webforj-bbjship?apps", "/webforj-bbjship"})
  void shouldRefuseEndpointThatIsNotPlainHttpOrHttps(String value) {
    URI endpoint = URI.create(value);

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, () -> BBjShipClient.create(endpoint));

    assertEquals("BBj deployment URL must be an HTTP or HTTPS URL without credentials, query or "
        + "fragment", failure.getMessage());
  }

  @ParameterizedTest
  @ValueSource(strings = {"https://build.example/webforj-bbjship",
      "http://localhost:8888/webforj-bbjship", "http://127.0.0.1:8888/webforj-bbjship",
      "http://[::1]:8888/webforj-bbjship", "http://[0:0:0:0:0:0:0:1]:8888/webforj-bbjship",
      "http://[::ffff:127.0.0.1]:8888/webforj-bbjship", "http://LOCALHOST/webforj-bbjship"})
  void shouldNotWarnForHttpsOrLoopbackEndpoint(String value) throws IOException {
    List<String> warnings = new ArrayList<>();
    BBjShipClient bbjShip = BBjShipClient.create(URI.create(value))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
        .setVersion(CLIENT_VERSION).setWarn(warnings::add).build();
    Path archive = temporary.resolve("hello-app.zip");
    String publishName = "hello-app";
    CloseableHttpClient offline = mock(CloseableHttpClient.class);
    when(offline.execute(any(ClassicHttpRequest.class), any(HttpContext.class), any()))
        .thenThrow(new IOException("offline"));
    HttpClientBuilder connections = mock(HttpClientBuilder.class, Answers.RETURNS_SELF);
    when(connections.build()).thenReturn(offline);

    try (MockedStatic<HttpClients> clients = mockStatic(HttpClients.class)) {
      clients.when(HttpClients::custom).thenReturn(connections);

      assertThrows(IOException.class, () -> bbjShip.deploy(archive, publishName));
      assertThrows(IOException.class, () -> bbjShip.undeploy(publishName));
      assertThrows(IOException.class, bbjShip::getVersion);
    }

    assertEquals(List.of(), warnings);
  }

  @Test
  void shouldNotWarnForPlainHttpEndpointAtAnAddressOfThisMachine() throws Exception {
    Optional<InetAddress> address = Collections.list(NetworkInterface.getNetworkInterfaces())
        .stream().flatMap(network -> Collections.list(network.getInetAddresses()).stream())
        .filter(candidate -> candidate instanceof Inet4Address && !candidate.isLoopbackAddress())
        .findFirst();
    assumeTrue(address.isPresent(), "This machine has no network interface address");
    URI endpoint = URI.create("http://" + address.get().getHostAddress() + ":8888/webforj-bbjship");
    List<String> warnings = new ArrayList<>();
    BBjShipClient bbjShip = BBjShipClient.create(endpoint)
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
        .setVersion(CLIENT_VERSION).setWarn(warnings::add).build();
    Path archive = temporary.resolve("hello-app.zip");
    String publishName = "hello-app";
    CloseableHttpClient offline = mock(CloseableHttpClient.class);
    when(offline.execute(any(ClassicHttpRequest.class), any(HttpContext.class), any()))
        .thenThrow(new IOException("offline"));
    HttpClientBuilder connections = mock(HttpClientBuilder.class, Answers.RETURNS_SELF);
    when(connections.build()).thenReturn(offline);

    try (MockedStatic<HttpClients> clients = mockStatic(HttpClients.class)) {
      clients.when(HttpClients::custom).thenReturn(connections);

      IOException undeployment =
          assertThrows(IOException.class, () -> bbjShip.undeploy(publishName));

      assertEquals("offline", undeployment.getMessage());
    }

    assertEquals(List.of(), warnings);
  }

  @Test
  void shouldRefuseToSendTheCredentialsOverPlainHttpToAnotherMachine() throws IOException {
    URI endpoint = URI.create("http://build.example:8888/webforj-bbjship");
    List<String> warnings = new ArrayList<>();
    BBjShipClient bbjShip = BBjShipClient.create(endpoint)
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
        .setVersion(CLIENT_VERSION).setWarn(warnings::add).build();
    Path archive = temporary.resolve("hello-app.zip");
    String publishName = "hello-app";
    CloseableHttpClient offline = mock(CloseableHttpClient.class);
    when(offline.execute(any(ClassicHttpRequest.class), any(HttpContext.class), any()))
        .thenThrow(new IOException("offline"));
    HttpClientBuilder connections = mock(HttpClientBuilder.class, Answers.RETURNS_SELF);
    when(connections.build()).thenReturn(offline);

    try (MockedStatic<HttpClients> clients = mockStatic(HttpClients.class)) {
      clients.when(HttpClients::custom).thenReturn(connections);

      IOException deployment =
          assertThrows(IOException.class, () -> bbjShip.deploy(archive, publishName));
      IOException undeployment =
          assertThrows(IOException.class, () -> bbjShip.undeploy(publishName));
      IOException check = assertThrows(IOException.class, bbjShip::getVersion);

      assertEquals(
          "BBj deployment URL http://build.example:8888/webforj-bbjship sends the "
              + "administration credentials over an unencrypted connection to another machine. "
              + "Use an https:// URL with the SSL port of the BBjServices web server",
          deployment.getMessage());
      assertEquals(
          "BBj undeployment URL http://build.example:8888/webforj-bbjship sends the "
              + "administration credentials over an unencrypted connection to another machine. "
              + "Use an https:// URL with the SSL port of the BBjServices web server",
          undeployment.getMessage());
      assertEquals(
          "BBjShip check URL http://build.example:8888/webforj-bbjship sends the "
              + "administration credentials over an unencrypted connection to another machine. "
              + "Use an https:// URL with the SSL port of the BBjServices web server",
          check.getMessage());
      clients.verifyNoInteractions();
    }

    verify(offline, never()).execute(any(ClassicHttpRequest.class), any(HttpContext.class), any());
    assertEquals(List.of(), warnings);
  }

  @Test
  void shouldWarnInPlaceOfRefusingWhenUnencryptedTransportIsAllowed() throws IOException {
    URI endpoint = URI.create("http://build.example:8888/webforj-bbjship");
    List<String> warnings = new ArrayList<>();
    BBjShipClient bbjShip = BBjShipClient.create(endpoint)
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
        .setVersion(CLIENT_VERSION).setWarn(warnings::add).setAllowUnencrypted(true).build();
    Path archive = temporary.resolve("hello-app.zip");
    String publishName = "hello-app";
    CloseableHttpClient offline = mock(CloseableHttpClient.class);
    when(offline.execute(any(ClassicHttpRequest.class), any(HttpContext.class), any()))
        .thenThrow(new IOException("offline"));
    HttpClientBuilder connections = mock(HttpClientBuilder.class, Answers.RETURNS_SELF);
    when(connections.build()).thenReturn(offline);

    try (MockedStatic<HttpClients> clients = mockStatic(HttpClients.class)) {
      clients.when(HttpClients::custom).thenReturn(connections);

      IOException deployment =
          assertThrows(IOException.class, () -> bbjShip.deploy(archive, publishName));
      IOException undeployment =
          assertThrows(IOException.class, () -> bbjShip.undeploy(publishName));

      assertEquals("BBj deployment archive is missing. Package the application first",
          deployment.getMessage());
      assertEquals("offline", undeployment.getMessage());
    }

    assertEquals(List.of(
        "BBj deployment URL http://build.example:8888/webforj-bbjship sends the "
            + "administration credentials over an unencrypted connection to another machine. "
            + "Use an https:// URL with the SSL port of the BBjServices web server",
        "BBj undeployment URL http://build.example:8888/webforj-bbjship sends the "
            + "administration credentials over an unencrypted connection to another machine. "
            + "Use an https:// URL with the SSL port of the BBjServices web server"),
        warnings);
  }

  @Test
  void shouldRefuseNullConfigurationAndReturnTheBuilderFromSetters() {
    BBjShipClient.Builder builder =
        BBjShipClient.create(URI.create("https://build.example/webforj-bbjship"));
    URI endpoint = null;
    assertThrows(NullPointerException.class, () -> BBjShipClient.create(endpoint));
    BBjCredentials credentials = null;
    assertThrows(NullPointerException.class, () -> builder.setCredentials(credentials));
    String version = null;
    assertThrows(NullPointerException.class, () -> builder.setVersion(version));
    Consumer<String> warn = null;
    assertThrows(NullPointerException.class, () -> builder.setWarn(warn));

    assertSame(builder,
        builder.setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)));
    assertSame(builder, builder.setVersion(CLIENT_VERSION));
    assertSame(builder, builder.setWarn(warnings -> {
    }));
    assertSame(builder, builder.setAllowUnencrypted(true));
  }

  @ParameterizedTest
  @MethodSource("getPorts")
  void shouldGiveThePortOfTheUrl(String url, int port) {
    assertEquals(port, BBjShipClient.getPort(URI.create(url)));
  }

  private static Stream<Arguments> getPorts() {
    return Stream.of(Arguments.of("http://localhost:8888/webforj-bbjship", 8888),
        Arguments.of("https://localhost/webforj-bbjship", 443),
        Arguments.of("http://localhost/webforj-bbjship", 80));
  }
}
