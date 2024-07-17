package com.webforj.component.terminal;

import com.google.gson.annotations.SerializedName;

/**
 * Represents the options that can be set for a terminal.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public class TerminalOptions {
  /**
   * An enum for the style of the cursor when the terminal is focused.
   */
  public enum CursorStyle {
    @SerializedName("block")
    BLOCK,

    @SerializedName("underline")
    UNDERLINE,

    @SerializedName("bar")
    BAR;
  }

  /**
   * An enum for rhe style of the cursor when the terminal is not focused.
   */
  public enum CursorInactiveStyle {
    @SerializedName("outline")
    OUTLINE,

    @SerializedName("block")
    BLOCK,

    @SerializedName("bar")
    BAR,

    @SerializedName("underline")
    UNDERLINE,

    @SerializedName("none")
    NONE;
  }

  /**
   * An enum which defines the modifier key hold to multiply scroll speed.
   */
  public enum FastScrollModifier {
    @SerializedName("none")
    NONE,

    @SerializedName("alt")
    ALT,

    @SerializedName("ctrl")
    CTRL,

    @SerializedName("shift")
    SHIFT;
  }

  private boolean cursorBlink = true;
  private CursorStyle cursorStyle = CursorStyle.BLOCK;
  private int cursorWidth = 1;
  private CursorInactiveStyle cursorInactiveStyle = CursorInactiveStyle.OUTLINE;
  private FastScrollModifier fastScrollModifier = FastScrollModifier.ALT;
  private int fastScrollSensitivity = 5;
  private String fontFamily = "\"Cascadia Code\", Menlo, monospace";
  private int fontSize = 14;
  private String fontWeight = "normal";
  private String fontWeightBold = "bold";
  private boolean ignoreBracketedPasteMode = false;
  private float lineHeight = 1;
  private int letterSpacing = 0;
  private int scrollback = 1000;
  private boolean scrollOnUserInput = true;
  private int scrollSensitivity = 1;
  private boolean screenReaderMode = false;
  private int smoothScrollDuration = 0;
  private boolean macOptionIsMeta = false;
  private boolean macOptionClickForcesSelection = false;
  private float minimumContrastRatio = 1;
  private int tabStopWidth = 8;
  private boolean rightClickSelectsWord = true;
  private boolean altClickMovesCursor = true;

  /**
   * Sets whether the cursor blinks.
   *
   * @param cursorBlink {@code true} if the cursor blinks, {@code false} otherwise
   * @return the terminal options itself
   */
  public TerminalOptions setCursorBlink(boolean cursorBlink) {
    this.cursorBlink = cursorBlink;
    return this;
  }

  /**
   * Gets whether the cursor blinks.
   *
   * @return {@code true} if the cursor blinks, {@code false} otherwise
   */
  public boolean isCursorBlink() {
    return cursorBlink;
  }

  /**
   * Sets the style of the cursor when the terminal is focused.
   *
   * @param cursorStyle the style of the cursor
   * @return the terminal options itself
   */
  public TerminalOptions setCursorStyle(CursorStyle cursorStyle) {
    this.cursorStyle = cursorStyle;
    return this;
  }

  /**
   * Gets the style of the cursor when the terminal is focused.
   *
   * @return the style of the cursor
   */
  public CursorStyle getCursorStyle() {
    return cursorStyle;
  }

  /**
   * Sets the width of the cursor.
   *
   * @param cursorWidth the width of the cursor
   * @return the terminal options itself
   */
  public TerminalOptions setCursorWidth(int cursorWidth) {
    this.cursorWidth = cursorWidth;
    return this;
  }

  /**
   * Gets the width of the cursor.
   *
   * @return the width of the cursor
   */
  public int getCursorWidth() {
    return cursorWidth;
  }

  /**
   * Sets the style of the cursor when the terminal is not focused.
   *
   * @param cursorInactiveStyle the style of the cursor
   * @return the terminal options itself
   */
  public TerminalOptions setCursorInactiveStyle(CursorInactiveStyle cursorInactiveStyle) {
    this.cursorInactiveStyle = cursorInactiveStyle;
    return this;
  }

  /**
   * Gets the style of the cursor when the terminal is not focused.
   *
   * @return the style of the cursor
   */
  public CursorInactiveStyle getCursorInactiveStyle() {
    return cursorInactiveStyle;
  }

  /**
   * Sets the modifier key hold to multiply scroll speed.
   *
   * @param fastScrollModifier the modifier key
   * @return the terminal options itself
   */
  public TerminalOptions setFastScrollModifier(FastScrollModifier fastScrollModifier) {
    this.fastScrollModifier = fastScrollModifier;
    return this;
  }

  /**
   * Gets the modifier key hold to multiply scroll speed.
   *
   * @return the modifier key
   */
  public FastScrollModifier getFastScrollModifier() {
    return fastScrollModifier;
  }

  /**
   * Sets The scroll speed multiplier used for fast scrolling.
   *
   * @param fastScrollSensitivity the scroll speed multiplier
   * @return the terminal options itself
   */
  public TerminalOptions setFastScrollSensitivity(int fastScrollSensitivity) {
    this.fastScrollSensitivity = fastScrollSensitivity;
    return this;
  }

  /**
   * Gets the scroll speed multiplier used for fast scrolling.
   *
   * @return the scroll speed multiplier
   */
  public int getFastScrollSensitivity() {
    return fastScrollSensitivity;
  }

  /**
   * Sets the font family.
   *
   * @param fontFamily the font family
   * @return the terminal options itself
   */
  public TerminalOptions setFontFamily(String fontFamily) {
    this.fontFamily = fontFamily;
    return this;
  }

  /**
   * Gets the font family.
   *
   * @return the font family
   */
  public String getFontFamily() {
    return fontFamily;
  }

  /**
   * Sets the font size.
   *
   * @param fontSize the font size
   * @return the terminal options itself
   */
  public TerminalOptions setFontSize(int fontSize) {
    this.fontSize = fontSize;
    return this;
  }

  /**
   * Gets the font size.
   *
   * @return the font size
   */
  public int getFontSize() {
    return fontSize;
  }

  /**
   * Sets the font weight.
   *
   * @param fontWeight the font weight
   * @return the terminal options itself
   */
  public TerminalOptions setFontWeight(String fontWeight) {
    this.fontWeight = fontWeight;
    return this;
  }

  /**
   * Gets the font weight.
   *
   * @return the font weight
   */
  public String getFontWeight() {
    return fontWeight;
  }

  /**
   * Sets the font weight for bold text.
   *
   * @param fontWeightBold the font weight for bold text
   * @return the terminal options itself
   */
  public TerminalOptions setFontWeightBold(String fontWeightBold) {
    this.fontWeightBold = fontWeightBold;
    return this;
  }

  /**
   * Gets the font weight for bold text.
   *
   * @return the font weight for bold text
   */
  public String getFontWeightBold() {
    return fontWeightBold;
  }

  /**
   * Sets whether to ignore the bracketed paste mode. When true, this will always paste without the
   * \x1b[200~ and \x1b[201~ sequences, even when the shell enables bracketed mode.
   *
   * @param ignoreBracketedPasteMode {@code true} to ignore the bracketed paste mode, {@code false}
   * @return the terminal options itself
   */
  public TerminalOptions setIgnoreBracketedPasteMode(boolean ignoreBracketedPasteMode) {
    this.ignoreBracketedPasteMode = ignoreBracketedPasteMode;
    return this;
  }

  /**
   * Gets whether to ignore the bracketed paste mode.
   *
   * @return {@code true} to ignore the bracketed paste mode, {@code false} otherwise
   */
  public boolean isIgnoreBracketedPasteMode() {
    return ignoreBracketedPasteMode;
  }

  /**
   * Sets the line height.
   *
   * @param lineHeight the line height
   * @return the terminal options itself
   */
  public TerminalOptions setLineHeight(float lineHeight) {
    this.lineHeight = lineHeight;
    return this;
  }

  /**
   * Gets the line height.
   *
   * @return the line height
   */
  public float getLineHeight() {
    return lineHeight;
  }

  /**
   * Sets the letter spacing.
   *
   * @param letterSpacing the letter spacing
   * @return the terminal options itself
   */
  public TerminalOptions setLetterSpacing(int letterSpacing) {
    this.letterSpacing = letterSpacing;
    return this;
  }

  /**
   * Gets the letter spacing.
   *
   * @return the letter spacing
   */
  public int getLetterSpacing() {
    return letterSpacing;
  }

  /**
   * Sets the amount of scrollback in the terminal.
   *
   * <p>
   * Scrollback is the amount of rows that are retained when lines are scrolled beyond the initial
   * viewport. Defaults to 1000.
   * </p>
   *
   * @param scrollback the scrollback
   * @return the terminal options itself
   */
  public TerminalOptions setScrollback(int scrollback) {
    this.scrollback = scrollback;
    return this;
  }

  /**
   * Gets the amount of scrollback in the terminal.
   *
   * @return the scrollback
   */
  public int getScrollback() {
    return scrollback;
  }

  /**
   * Sets whether to scroll to the bottom whenever there is some user input. The default is true.
   *
   * @param scrollOnUserInput {@code true} to scroll to the bottom whenever there is some user
   *        input, {@code false} otherwise
   * @return the terminal options itself
   */
  public TerminalOptions setScrollOnUserInput(boolean scrollOnUserInput) {
    this.scrollOnUserInput = scrollOnUserInput;
    return this;
  }

  /**
   * Gets whether to scroll to the bottom whenever there is some user input.
   *
   * @return {@code true} to scroll to the bottom whenever there is some user input, {@code false}
   *         otherwise
   */
  public boolean isScrollOnUserInput() {
    return scrollOnUserInput;
  }

  /**
   * Sets The scrolling speed multiplier used for adjusting normal scrolling speed.
   *
   * @param scrollSensitivity the scrolling speed multiplier
   * @return the terminal options itself
   */
  public TerminalOptions setScrollSensitivity(int scrollSensitivity) {
    this.scrollSensitivity = scrollSensitivity;
    return this;
  }

  /**
   * Gets the scrolling speed multiplier used for adjusting normal scrolling speed.
   *
   * @return the scrolling speed multiplier
   */
  public int getScrollSensitivity() {
    return scrollSensitivity;
  }

  /**
   * Sets whether screen reader support is enabled. When on this will expose supporting elements in
   * the DOM to support NVDA on Windows and VoiceOver on macOS.
   *
   * @param screenReaderMode {@code true} to enable screen reader support, {@code false} otherwise
   * @return the terminal options itself
   */
  public TerminalOptions setScreenReaderMode(boolean screenReaderMode) {
    this.screenReaderMode = screenReaderMode;
    return this;
  }

  /**
   * Gets whether screen reader support is enabled.
   *
   * @return {@code true} to enable screen reader support, {@code false} otherwise
   */
  public boolean isScreenReaderMode() {
    return screenReaderMode;
  }

  /**
   * Sets the duration to smoothly scroll between the origin and the target in milliseconds. Set to
   * 0 to disable smooth scrolling and scroll instantly.
   *
   * @param smoothScrollDuration the duration to smoothly scroll between the origin and the target
   * @return the terminal options itself
   */
  public TerminalOptions setSmoothScrollDuration(int smoothScrollDuration) {
    this.smoothScrollDuration = smoothScrollDuration;
    return this;
  }

  /**
   * Gets the duration to smoothly scroll between the origin and the target in milliseconds.
   *
   * @return the duration to smoothly scroll between the origin and the target
   */
  public int getSmoothScrollDuration() {
    return smoothScrollDuration;
  }

  /**
   * Sets whether to treat option as the meta key.
   *
   * @param macOptionIsMeta {@code true} if the option key acts as a meta key, {@code false}
   *        otherwise
   * @return the terminal options itself
   */
  public TerminalOptions setMacOptionIsMeta(boolean macOptionIsMeta) {
    this.macOptionIsMeta = macOptionIsMeta;
    return this;
  }

  /**
   * Gets whether to treat option as the meta key.
   *
   * @return {@code true} if the option key acts as a meta key, {@code false} otherwise
   */
  public boolean isMacOptionIsMeta() {
    return macOptionIsMeta;
  }

  /**
   * Sets whether holding a modifier key will force normal selection behavior, regardless of whether
   * the terminal is in mouse events mode. This will also prevent mouse events from being emitted by
   * the terminal. For example, this allows you to use xterm.js’ regular selection inside tmux with
   * mouse mode enabled.
   *
   * @param macOptionClickForcesSelection {@code true} to force selection on right click, {@code
   *        false} otherwise
   * @return the terminal options itself
   */
  public TerminalOptions setMacOptionClickForcesSelection(boolean macOptionClickForcesSelection) {
    this.macOptionClickForcesSelection = macOptionClickForcesSelection;
    return this;
  }

  /**
   * Gets whether holding a modifier key will force normal selection behavior, regardless of whether
   * the terminal is in mouse events mode.
   *
   * @return {@code true} to force selection on right click, {@code false} otherwise
   */
  public boolean isMacOptionClickForcesSelection() {
    return macOptionClickForcesSelection;
  }

  /**
   * The minimum contrast ratio for text in the terminal.
   *
   * <p>
   * setting this will change the foreground color dynamically depending on whether the contrast
   * ratio is met. Example values:
   *
   * <ul>
   * <li>1: The default, do nothing.
   * <li>4.5: Minimum for WCAG AA compliance.
   * <li>7: Minimum for WCAG AAA compliance.
   * <li>21: White on black or black on white.
   * </ul>
   * </p>
   *
   * @param minimumContrastRatio the minimum contrast ratio
   * @return the terminal options itself
   */
  public TerminalOptions setMinimumContrastRatio(float minimumContrastRatio) {
    this.minimumContrastRatio = minimumContrastRatio;
    return this;
  }

  /**
   * Gets the minimum contrast ratio for text in the terminal.
   *
   * @return the minimum contrast ratio
   */
  public float getMinimumContrastRatio() {
    return minimumContrastRatio;
  }

  /**
   * Sets the size of tab stops in the terminal.
   *
   * @param tabStopWidth the tab stop width
   * @return the terminal options itself
   */
  public TerminalOptions setTabStopWidth(int tabStopWidth) {
    this.tabStopWidth = tabStopWidth;
    return this;
  }

  /**
   * Gets the size of tab stops in the terminal.
   *
   * @return the tab stop width
   */
  public int getTabStopWidth() {
    return tabStopWidth;
  }

  /**
   * Sets whether to select the word under the cursor on right click, this is standard behavior in a
   * lot of macOS applications.
   *
   * @param rightClickSelectsWord {@code true} to select the word under the cursor on right click,
   *        {@code false} otherwise
   * @return the terminal options itself
   */
  public TerminalOptions setRightClickSelectsWord(boolean rightClickSelectsWord) {
    this.rightClickSelectsWord = rightClickSelectsWord;
    return this;
  }

  /**
   * Gets whether to select the word under the cursor on right click.
   *
   * @return {@code true} to select the word under the cursor on right click, {@code false}
   *         otherwise
   */
  public boolean isRightClickSelectsWord() {
    return rightClickSelectsWord;
  }

  /**
   * If enabled, alt + click will move the prompt cursor to position underneath the mouse. The
   * default is true.
   *
   * @param altClickMovesCursor {@code true} if alt + click moves the prompt cursor to position
   *        underneath the mouse, {@code false} otherwise
   * @return the terminal options itself
   */
  public TerminalOptions setAltClickMovesCursor(boolean altClickMovesCursor) {
    this.altClickMovesCursor = altClickMovesCursor;
    return this;
  }

  /**
   * Gets whether alt + click moves the prompt cursor to position underneath the mouse.
   *
   * @return {@code true} if alt + click moves the prompt cursor to position underneath the mouse,
   *         {@code false} otherwise
   */
  public boolean isAltClickMovesCursor() {
    return altClickMovesCursor;
  }
}
