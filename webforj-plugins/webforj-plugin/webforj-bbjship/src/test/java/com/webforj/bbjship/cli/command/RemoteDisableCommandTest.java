package com.webforj.bbjship.cli.command;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import com.webforj.bbjship.cli.BBjShipCli;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;
import picocli.CommandLine.Help.Ansi;

class RemoteDisableCommandTest {

  @TempDir
  Path temporary;

  @Test
  void shouldRemoveTheBbjShipContext() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    services.addContext("webforj-bbjship", """
        {
          "STOP": {"EXECUTOR": "CONTEXT:webforj-bbjship", "DATA": {"ACTION": "STOP"},
            "ENABLED": false},
          "Document Base": "%s"
        }""".formatted(home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();

    int status = cli.run("remote", "disable");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Disabled the BBjShip HTTP endpoint\n",
        output.toString(StandardCharsets.UTF_8));
    InOrder calls = inOrder(services.getServer(), services.getAdmin());
    calls.verify(services.getServer()).removeContext("webforj-bbjship");
    calls.verify(services.getServer()).commit();
    calls.verify(services.getAdmin()).release();
  }

  @Test
  void shouldSayWhenTheBbjShipContextDoesNotExist() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();

    int status = cli.run("remote", "disable");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "The BBjShip HTTP endpoint is not enabled\n",
        output.toString(StandardCharsets.UTF_8));
    verify(services.getServer(), never()).removeContext(anyString());
  }
}
