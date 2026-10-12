package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjShipInputTest {

  @ParameterizedTest
  @MethodSource("getLines")
  void shouldReadTheLineWithoutItsTerminator(String text, String expected) throws Exception {
    InputStream input = new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8));

    assertEquals(expected, BBjShipInput.readLine(input));
  }

  @Test
  void shouldReadEachLineOfTheSameStreamInTurn() throws Exception {
    InputStream input = new ByteArrayInputStream("admin123\ny\n".getBytes(StandardCharsets.UTF_8));

    assertEquals("admin123", BBjShipInput.readLine(input));
    assertEquals("y", BBjShipInput.readLine(input));
    assertNull(BBjShipInput.readLine(input));
  }

  @Test
  void shouldReturnNullAtTheEndOfTheStream() throws Exception {
    InputStream input = InputStream.nullInputStream();

    assertNull(BBjShipInput.readLine(input));
  }

  private static Stream<Arguments> getLines() {
    return Stream.of(Arguments.of("secret\n", "secret"), Arguments.of("secret\r\n", "secret"),
        Arguments.of("\n", ""), Arguments.of("geheim ü ✓ 密码\n", "geheim ü ✓ 密码"),
        Arguments.of("secret", "secret"), Arguments.of("x".repeat(100) + "\n", "x".repeat(100)));
  }
}
