package com.webforj.bbjship.cli.command;

import static com.webforj.bbjship.foundation.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.verify;

import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.TerminalLines;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;
import picocli.CommandLine.Help.Ansi;

class ServicesStopCommandTest {

  private static final String SERVICES_GROUP = "services";

  @TempDir
  Path temporary;

  private BBjServices bbjServices;
  private ByteArrayOutputStream output;
  private BBjShipCli cli;

  @BeforeEach
  void initCliOnTheBBjHomeFixture() throws IOException, URISyntaxException, BBjAdminException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    bbjServices = new BBjServices();
    bbjServices.addBBjShipConfiguration(home);
    output = new ByteArrayOutputStream();
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
        .setAnsi(Ansi.OFF).setSessions(login -> bbjServices.getAdmin()).build();
  }

  @Test
  void shouldAskBbjServicesToShutDownAndReportTheStop() throws Exception {
    int status = cli.run(SERVICES_GROUP, "stop");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Stopping BBjServices\n",
        output.toString(StandardCharsets.UTF_8));
    InOrder order = inOrder(bbjServices.getAdmin());
    order.verify(bbjServices.getAdmin()).shutdownBBjServices(false);
    order.verify(bbjServices.getAdmin()).release();
  }

  @Test
  void shouldReportTheStopBeforeTheShutdownRequest() throws Exception {
    List<String> printed = new ArrayList<>();
    doAnswer(invocation -> printed.add(output.toString(StandardCharsets.UTF_8)))
        .when(bbjServices.getAdmin()).shutdownBBjServices(false);

    int status = cli.run(SERVICES_GROUP, "stop");

    assertEquals(0, status);
    assertEquals(List.of(FACTORY_ACCOUNT_WARNING + "\n" + "Stopping BBjServices\n"), printed);
  }

  @Test
  void shouldReleaseTheSessionWhenBbjServicesRefusesTheShutdown() throws Exception {
    doThrow(new BBjAdminException("Shutdown refused")).when(bbjServices.getAdmin())
        .shutdownBBjServices(false);

    int status = cli.run(SERVICES_GROUP, "stop");

    assertEquals(1, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Stopping BBjServices\n"
        + "Command failed. Shutdown refused\n", output.toString(StandardCharsets.UTF_8));
    verify(bbjServices.getAdmin()).release();
  }
}
