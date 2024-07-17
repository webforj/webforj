package com.webforj.component.terminal;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TerminalThemeTest {
  TerminalTheme theme;

  @BeforeEach
  void setUp() {
    theme = new TerminalTheme();
  }

  @Test
  void shouldSetAndGetBlack() {
    theme.setBlack("#010101");
    assertEquals("#010101", theme.getBlack());
  }

  @Test
  void shouldSetAndGetRed() {
    theme.setRed("#ff0000");
    assertEquals("#ff0000", theme.getRed());
  }

  @Test
  void shouldSetAndGetGreen() {
    theme.setGreen("#00ff00");
    assertEquals("#00ff00", theme.getGreen());
  }

  @Test
  void shouldSetAndGetYellow() {
    theme.setYellow("#ffff00");
    assertEquals("#ffff00", theme.getYellow());
  }

  @Test
  void shouldSetAndGetBlue() {
    theme.setBlue("#0000ff");
    assertEquals("#0000ff", theme.getBlue());
  }

  @Test
  void shouldSetAndGetCyan() {
    theme.setCyan("#00ffff");
    assertEquals("#00ffff", theme.getCyan());
  }

  @Test
  void shouldSetAndGetWhite() {
    theme.setWhite("#ffffff");
    assertEquals("#ffffff", theme.getWhite());
  }

  @Test
  void shouldSetAndGetBrightBlack() {
    theme.setBrightBlack("#1a1a1a");
    assertEquals("#1a1a1a", theme.getBrightBlack());
  }

  @Test
  void shouldSetAndGetBrightRed() {
    theme.setBrightRed("#ff4c4c");
    assertEquals("#ff4c4c", theme.getBrightRed());
  }

  @Test
  void shouldSetAndGetBrightGreen() {
    theme.setBrightGreen("#00ff00");
    assertEquals("#00ff00", theme.getBrightGreen());
  }

  @Test
  void shouldSetAndGetBrightYellow() {
    theme.setBrightYellow("#ffff4c");
    assertEquals("#ffff4c", theme.getBrightYellow());
  }

  @Test
  void shouldSetAndGetBrightBlue() {
    theme.setBrightBlue("#4c4cff");
    assertEquals("#4c4cff", theme.getBrightBlue());
  }

  @Test
  void shouldSetAndGetBrightCyan() {
    theme.setBrightCyan("#4cffff");
    assertEquals("#4cffff", theme.getBrightCyan());
  }

  @Test
  void shouldSetAndGetBrightWhite() {
    theme.setBrightWhite("#ffffff");
    assertEquals("#ffffff", theme.getBrightWhite());
  }

  @Test
  void shouldSetAndGetBackground() {
    theme.setBackground("#1e1e1e");
    assertEquals("#1e1e1e", theme.getBackground());
  }

  @Test
  void shouldSetAndGetForeground() {
    theme.setForeground("#cccccc");
    assertEquals("#cccccc", theme.getForeground());
  }

  @Test
  void shouldSetAndGetSelectionBackground() {
    theme.setSelectionBackground("#3a3d41");
    assertEquals("#3a3d41", theme.getSelectionBackground());
  }

  @Test
  void shouldSetAndGetCursor() {
    theme.setCursor("#ffffff");
    assertEquals("#ffffff", theme.getCursor());
  }
}
