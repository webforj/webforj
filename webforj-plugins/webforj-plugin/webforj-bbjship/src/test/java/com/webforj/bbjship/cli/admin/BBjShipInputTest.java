package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.bbjship.cli.TerminalLines;
import java.io.StringWriter;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;

class BBjShipInputTest {

  private static final String PASSWORD = "secret";

  @Test
  void shouldReadEachTypedLineInTurnAndNothingAtTheEnd() {
    BBjShipInput input =
        new BBjShipInput(new TerminalLines(new StringWriter()).setTyped("admin123", "y"));

    assertEquals(Optional.of("admin123"), input.readLine());
    assertEquals(Optional.of("y"), input.readLine());
    assertEquals(Optional.empty(), input.readLine());
  }

  @Test
  void shouldPrintThePromptBeforeReadingThePassword() {
    StringWriter printed = new StringWriter();
    TerminalLines terminal = new TerminalLines(printed).setTyped(PASSWORD);
    BBjShipInput input = new BBjShipInput(terminal);

    Optional<String> password = input.readPassword("Enter the password");

    assertEquals(Optional.of(PASSWORD), password);
    assertEquals("Enter the password\n", printed.toString());
    assertEquals(List.of(PASSWORD), terminal.getPasswordReads());
    assertEquals(Optional.empty(), input.readPassword("Enter the password"));
  }

  @Test
  void shouldFindOnlyTheVariablesTheTerminalSets() {
    BBjShipInput input = new BBjShipInput(new TerminalLines(new StringWriter())
        .setVariables(Map.of("WEBFORJ_BBJ_USERNAME", "operator")));

    assertEquals(Optional.of("operator"), input.findVariable("WEBFORJ_BBJ_USERNAME"));
    assertEquals(Optional.empty(), input.findVariable("WEBFORJ_BBJ_PASSWORD"));
  }
}
