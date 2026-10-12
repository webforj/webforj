package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjShipContextsPortsTest extends EmptyWebAppServer {

  @ParameterizedTest
  @MethodSource("getBBjShipContexts")
  void shouldReadThePortsOfTheBbjShipContext(List<String> contextNames, String data,
      List<Integer> expected) throws Exception {
    when(server.getContextNames()).thenReturn(new BBjAdminAdapterList<>(contextNames));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(data.formatted(home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));

    List<Integer> ports = contexts.getPorts();

    assertEquals(expected, ports);
  }

  private static Stream<Arguments> getBBjShipContexts() {
    return Stream.of(Arguments.of(List.of("root"), """
        {"HTTP port": 8888, "SSL port": 8443, "Document Base": "%s"}""", List.of()),
        Arguments.of(List.of("root", "webforj-bbjship"), """
            {"HTTP port": 8888, "SSL port": 8443, "Document Base": "%s"}""", List.of(8888, 8443)),
        Arguments.of(List.of("root", "webforj-bbjship"), """
            {"HTTP port": 8888, "SSL port": "", "Document Base": "%s"}""", List.of(8888)));
  }
}
