package com.webforj.bbjship.cli.admin;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * Reads the lines a BBjShip command takes from its standard input.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipInput {

  private BBjShipInput() {}

  /**
   * Reads the next line of the input, leaving every byte after the line in the input.
   *
   * @param input the standard input of the command
   *
   * @return the line without its line terminator, or null when the input has ended
   * @throws IOException if the input cannot be read
   */
  public static String readLine(InputStream input) throws IOException {
    int next = input.read();
    if (next == -1) {
      return null;
    }

    byte[] line = new byte[64];
    int length = 0;
    while (next != -1 && next != '\n') {
      if (length == line.length) {
        line = Arrays.copyOf(line, length * 2);
      }

      line[length++] = (byte) next;
      next = input.read();
    }

    if (length > 0 && line[length - 1] == '\r') {
      length--;
    }

    return new String(line, 0, length, StandardCharsets.UTF_8);
  }
}
