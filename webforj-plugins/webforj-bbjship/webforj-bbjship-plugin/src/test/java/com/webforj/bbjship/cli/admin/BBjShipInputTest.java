package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.bbjship.cli.TerminalLines;
import java.io.StringWriter;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjShipInputTest {

  @Test
  void shouldReadEachTypedLineInTurnAndNothingAtTheEnd() {
    BBjShipInput input =
        new BBjShipInput(new TerminalLines(new StringWriter()).setTyped("admin123", "y"));

    assertEquals(Optional.of("admin123"), input.readLine());
    assertEquals(Optional.of("y"), input.readLine());
    assertEquals(Optional.empty(), input.readLine());
  }

  @ParameterizedTest
  @MethodSource("getOperatingSystems")
  void shouldOfferStandardInputOnlyOutsideWindows(String operatingSystem, boolean available) {
    BBjShipInput input = new BBjShipInput(new TerminalLines(new StringWriter()));
    input.setOperatingSystem(operatingSystem);

    assertEquals(available, input.isStandardInputAvailable());
  }

  @Test
  void shouldFindOnlyTheVariablesTheTerminalSets() {
    BBjShipInput input = new BBjShipInput(new TerminalLines(new StringWriter())
        .setVariables(Map.of("WEBFORJ_BBJ_USERNAME", "operator")));

    assertEquals(Optional.of("operator"), input.findVariable("WEBFORJ_BBJ_USERNAME"));
    assertEquals(Optional.empty(), input.findVariable("WEBFORJ_BBJ_PASSWORD"));
  }

  private static Stream<Arguments> getOperatingSystems() {
    return Stream.of(Arguments.of("Windows 11", false), Arguments.of("Windows Server 2022", false),
        Arguments.of("Linux", true), Arguments.of("Mac OS X", true));
  }
}
