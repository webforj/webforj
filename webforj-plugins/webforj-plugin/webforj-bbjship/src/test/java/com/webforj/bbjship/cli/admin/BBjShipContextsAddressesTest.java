package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import java.net.URI;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class BBjShipContextsAddressesTest extends EmptyWebAppServer {

  @ParameterizedTest
  @MethodSource("getUnsetSslPorts")
  void shouldReadUnsetSslPortAsNoHttpsAddress(String sslPort) throws Exception {
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": 8888, %s"Document Base": "%s"}""".formatted(sslPort,
        home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));

    List<URI> addresses = contexts.getAddresses();

    assertEquals(List.of(URI.create("http://localhost:8888/webforj-bbjship")), addresses);
  }

  private static Stream<String> getUnsetSslPorts() {
    return Stream.of("", "\"SSL port\": \"\", ", "\"SSL port\": \"none\", ", "\"SSL port\": -1, ",
        "\"SSL port\": 0, ", "\"SSL port\": null, ");
  }
}
