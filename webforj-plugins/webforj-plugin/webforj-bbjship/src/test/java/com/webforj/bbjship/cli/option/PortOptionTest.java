package com.webforj.bbjship.cli.option;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.admin.BBjShipPlugin;
import com.webforj.bbjship.cli.admin.BBjShipServices;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.command.Home;
import com.webforj.bbjship.cli.command.RemoteEnableCommand;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.OptionalInt;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import picocli.CommandLine;
import picocli.CommandLine.Help.Ansi;

class PortOptionTest {

  @TempDir
  Path temporary;

  private Path home;
  private ByteArrayOutputStream output;
  private BBjAdminBase admin;
  private List<String> credentials;
  private BBjShipCli cli;

  @BeforeEach
  void initCliOnTheBBjHomeFixture() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    output = new ByteArrayOutputStream();
    admin = mock(BBjAdminBase.class);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    credentials = new ArrayList<>();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream()).setEnvironment(Map.of())
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF).setConnector((username, password) -> {
          credentials.add(username);
          credentials.add(password);
          return admin;
        }).build();
  }

  @Test
  void shouldUseTheDefaultPortOfTheWebServerWithoutPortOption() {
    CommandLine line = new CommandLine(new RemoteEnableCommand(
        new BBjShipSession(new BBjShipPlugin(home), InputStream.nullInputStream(), Map.of(),
            (username, password) -> admin, new BBjShipServices(new BBjShipPlugin(home)))));

    line.parseArgs();

    PortOption option = (PortOption) line.getCommandSpec().mixins().get("port").userObject();
    assertEquals(8888, option.getPort());
  }

  @ParameterizedTest
  @MethodSource("getPortOptions")
  void shouldReadTheHttpAndTheSslPort(List<String> arguments, int httpPort, OptionalInt sslPort) {
    CommandLine line = new CommandLine(new RemoteEnableCommand(
        new BBjShipSession(new BBjShipPlugin(home), InputStream.nullInputStream(), Map.of(),
            (username, password) -> admin, new BBjShipServices(new BBjShipPlugin(home)))));

    line.parseArgs(arguments.toArray(String[]::new));

    PortOption option = (PortOption) line.getCommandSpec().mixins().get("port").userObject();
    assertEquals(httpPort, option.getPort());
    assertEquals(sslPort, option.getSslPort());
  }

  @ParameterizedTest
  @MethodSource("invalidPorts")
  void shouldRefuseMissingOrInvalidPort(List<String> arguments, String message) {
    int status = cli.run(arguments.toArray(String[]::new));

    assertEquals(2, status);
    assertEquals(message + "\n", output.toString(StandardCharsets.UTF_8));
    assertTrue(credentials.isEmpty());
  }

  private static Stream<Arguments> invalidPorts() {
    return Stream.of(
        Arguments.of(List.of("remote", "enable", "--port", "web"),
            "Specify --port from 1 to 65535"),
        Arguments.of(List.of("remote", "enable", "--port", "65536"),
            "Specify --port from 1 to 65535"),
        Arguments.of(List.of("remote", "enable", "--port"), "Specify a port after --port"),
        Arguments.of(List.of("remote", "enable", "--ssl-port", "0"),
            "Specify --ssl-port from 1 to 65535"),
        Arguments.of(List.of("remote", "enable", "--ssl-port"), "Specify a port after --ssl-port"));
  }

  private static Stream<Arguments> getPortOptions() {
    return Stream.of(Arguments.of(List.of("--ssl-port", "8443"), 8888, OptionalInt.of(8443)),
        Arguments.of(List.of("--port", "8080", "--ssl-port", "8443"), 8080, OptionalInt.of(8443)));
  }
}
