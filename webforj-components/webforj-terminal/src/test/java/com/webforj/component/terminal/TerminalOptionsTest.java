package com.webforj.component.terminal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TerminalOptionsTest {
  TerminalOptions options;

  @BeforeEach
  void setUp() {
    options = new TerminalOptions();
  }

  @Test
  void shouldSetAndGetCursorBlink() {
    options.setCursorBlink(false);
    assertFalse(options.isCursorBlink());
  }

  @Test
  void shouldSetAndGetCursorStyle() {
    options.setCursorStyle(TerminalOptions.CursorStyle.UNDERLINE);
    assertEquals(TerminalOptions.CursorStyle.UNDERLINE, options.getCursorStyle());
  }

  @Test
  void shouldSetAndGetCursorWidth() {
    options.setCursorWidth(3);
    assertEquals(3, options.getCursorWidth());
  }

  @Test
  void shouldSetAndGetCursorInactiveStyle() {
    options.setCursorInactiveStyle(TerminalOptions.CursorInactiveStyle.BAR);
    assertEquals(TerminalOptions.CursorInactiveStyle.BAR, options.getCursorInactiveStyle());
  }

  @Test
  void shouldSetAndGetFastScrollModifier() {
    options.setFastScrollModifier(TerminalOptions.FastScrollModifier.CTRL);
    assertEquals(TerminalOptions.FastScrollModifier.CTRL, options.getFastScrollModifier());
  }

  @Test
  void shouldSetAndGetFastScrollSensitivity() {
    options.setFastScrollSensitivity(10);
    assertEquals(10, options.getFastScrollSensitivity());
  }

  @Test
  void shouldSetAndGetFontFamily() {
    options.setFontFamily("Arial");
    assertEquals("Arial", options.getFontFamily());
  }

  @Test
  void shouldSetAndGetFontSize() {
    options.setFontSize(16);
    assertEquals(16, options.getFontSize());
  }

  @Test
  void shouldSetAndGetFontWeight() {
    options.setFontWeight("bold");
    assertEquals("bold", options.getFontWeight());
  }

  @Test
  void shouldSetAndGetFontWeightBold() {
    options.setFontWeightBold("900");
    assertEquals("900", options.getFontWeightBold());
  }

  @Test
  void shouldSetAndGetIgnoreBracketedPasteMode() {
    options.setIgnoreBracketedPasteMode(true);
    assertTrue(options.isIgnoreBracketedPasteMode());
  }

  @Test
  void shouldSetAndGetLineHeight() {

    options.setLineHeight(1.5f);
    assertEquals(1.5f, options.getLineHeight());
  }

  @Test
  void shouldSetAndGetLetterSpacing() {
    options.setLetterSpacing(2);
    assertEquals(2, options.getLetterSpacing());
  }

  @Test
  void shouldSetAndGetScrollback() {
    options.setScrollback(500);
    assertEquals(500, options.getScrollback());
  }

  @Test
  void shouldSetAndGetScrollOnUserInput() {
    options.setScrollOnUserInput(false);
    assertFalse(options.isScrollOnUserInput());
  }

  @Test
  void shouldSetAndGetScrollSensitivity() {
    options.setScrollSensitivity(3);
    assertEquals(3, options.getScrollSensitivity());
  }

  @Test
  void shouldSetAndGetScreenReaderMode() {

    options.setScreenReaderMode(true);
    assertTrue(options.isScreenReaderMode());
  }

  @Test
  void shouldSetAndGetSmoothScrollDuration() {
    options.setSmoothScrollDuration(300);
    assertEquals(300, options.getSmoothScrollDuration());
  }

  @Test
  void shouldSetAndGetMacOptionIsMeta() {
    options.setMacOptionIsMeta(true);
    assertTrue(options.isMacOptionIsMeta());
  }

  @Test
  void shouldSetAndGetMacOptionClickForcesSelection() {
    options.setMacOptionClickForcesSelection(true);
    assertTrue(options.isMacOptionClickForcesSelection());
  }

  @Test
  void shouldSetAndGetMinimumContrastRatio() {
    options.setMinimumContrastRatio(7.0f);
    assertEquals(7.0f, options.getMinimumContrastRatio());
  }

  @Test
  void shouldSetAndGetTabStopWidth() {
    options.setTabStopWidth(4);
    assertEquals(4, options.getTabStopWidth());
  }

  @Test
  void shouldSetAndGetRightClickSelectsWord() {
    options.setRightClickSelectsWord(false);
    assertFalse(options.isRightClickSelectsWord());
  }

  @Test
  void shouldSetAndGetAltClickMovesCursor() {
    options.setAltClickMovesCursor(false);
    assertFalse(options.isAltClickMovesCursor());
  }
}
