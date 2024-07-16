package com.webforj.component.terminal;

import com.webforj.App;
import com.webforj.PendingResult;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.terminal.events.TerminalDataEvent;
import com.webforj.component.terminal.events.TerminalKeyEvent;
import com.webforj.concern.HasSize;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * Represents a terminal component that can be used to interact with a terminal emulator.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@NodeName("dwc-terminal")
public class Terminal extends ElementComposite implements HasSize<Terminal> {
  private int rows;
  private int cols;
  private boolean autoFit;

  static final String JS_METHOD_SET_FOCUS = "setFocus";
  static final String JS_METHOD_REMOVE_FOCUS = "removeFocus";
  static final String JS_METHOD_WRITE = "write";
  static final String JS_METHOD_WRITELN = "writeln";
  static final String JS_METHOD_CLEAR = "clear";
  static final String JS_METHOD_RESET = "reset";
  static final String JS_METHOD_FIT = "fit";
  static final String JS_METHOD_SELECT_ALL = "selectAll";
  static final String JS_METHOD_SELECT = "select";
  static final String JS_METHOD_SELECT_LINES = "selectLines";
  static final String JS_METHOD_CLEAR_SELECTION = "clearSelection";
  static final String JS_METHOD_GET_SELECTION = "getSelection";
  static final String JS_METHOD_SCROLL_LINES = "scrollLines";
  static final String JS_METHOD_SCROLL_TO_LINE = "scrollToLine";
  static final String JS_METHOD_SCROLL_TO_TOP = "scrollToTop";
  static final String JS_METHOD_SCROLL_TO_BOTTOM = "scrollToBottom";

  // Property descriptors
  private final PropertyDescriptor<Integer> rowsProp = PropertyDescriptor.property("rows", 24);
  private final PropertyDescriptor<Integer> colsProp = PropertyDescriptor.property("cols", 80);
  private final PropertyDescriptor<Boolean> autoFitProp =
      PropertyDescriptor.property("autoFit", false);
  private final PropertyDescriptor<TerminalOptions> optionsProp =
      PropertyDescriptor.property("options", new TerminalOptions());
  private final PropertyDescriptor<TerminalTheme> themeProp =
      PropertyDescriptor.property("theme", new TerminalTheme());

  /**
   * Sets the number of rows.
   *
   * @param rows the number of rows
   * @return the component itself
   */
  public Terminal setRows(int rows) {
    this.rows = rows;
    set(rowsProp, rows);
    return this;
  }

  /**
   * Gets the number of rows.
   *
   * @return the number of rows
   */
  public int getRows() {
    return rows;
  }

  /**
   * Sets the number of columns.
   *
   * @param cols the number of columns
   * @return the component itself
   */
  public Terminal setCols(int cols) {
    this.cols = cols;
    set(colsProp, cols);
    return this;
  }

  /**
   * Gets the number of columns.
   *
   * @return the number of columns
   */
  public int getCols() {
    return cols;
  }

  /**
   * Sets the auto fit property.
   *
   * <p>
   * The property controls whether to automatically fit the terminal to its container
   * </p>
   *
   * @param autoFit {@code true} to automatically fit the terminal to its container, {@code false}
   *        otherwise
   * @return the component itself
   */
  public Terminal setAutoFit(boolean autoFit) {
    this.autoFit = autoFit;
    set(autoFitProp, autoFit);
    return this;
  }

  /**
   * Checks whether the terminal will automatically fit to its container.
   *
   * @return {@code true} if the terminal will automatically fit to its container, {@code false}
   *         otherwise
   */
  public boolean isAutoFit() {
    return autoFit;
  }

  /**
   * Sets the terminal options.
   *
   * @param options the terminal options
   * @return the component itself
   */
  public Terminal setOptions(TerminalOptions options) {
    set(optionsProp, options);
    return this;
  }

  /**
   * Gets the terminal options.
   *
   * @return the terminal options
   */
  public TerminalOptions getOptions() {
    return get(optionsProp);
  }

  /**
   * Sets the terminal theme.
   *
   * @param theme the terminal theme
   * @return the component itself
   */
  public Terminal setTheme(TerminalTheme theme) {
    set(themeProp, theme);
    return this;
  }

  /**
   * Gets the terminal theme.
   *
   * @return the terminal theme
   */
  public TerminalTheme getTheme() {
    return get(themeProp);
  }

  /**
   * Focuses the terminal.
   *
   * @return the component itself
   */
  public Terminal focus() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_SET_FOCUS);
    return this;
  }

  /**
   * Removes the focus from the terminal.
   *
   * @return the component itself
   */
  public Terminal blur() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_REMOVE_FOCUS);
    return this;
  }

  /**
   * Writes data to the terminal.
   *
   * @param data the data to write
   * @return a pending result for the operation
   */
  public PendingResult<Object> write(Object data) {
    return getOriginalElement().callJsFunctionAsync(JS_METHOD_WRITE, data);
  }

  /**
   * Writes data to the terminal followed by a new line.
   *
   * @param data the data to write
   * @return a pending result for the operation
   */
  public PendingResult<Object> writeln(Object data) {
    return getOriginalElement().callJsFunctionAsync(JS_METHOD_WRITELN, data);
  }

  /**
   * Clears the entire buffer, making the prompt line the new first line.
   *
   * @return the component itself
   */
  public Terminal clear() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_CLEAR);
    return this;
  }

  /**
   * Performs a full reset (RIS, aka ‘\x1bc’).
   *
   * @return the component itself
   */
  public Terminal reset() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_RESET);
    return this;
  }

  /**
   * Resizes the terminal to fill its container.
   *
   * <p>
   * If {@code autoFit} is true, this method is called automatically when the terminal's container
   * resizes.
   * </p>
   *
   * @return the component itself
   */
  public Terminal fit() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_FIT);
    return this;
  }

  /**
   * Selects all text within the terminal.
   *
   * @return the component itself
   */
  public Terminal selectAll() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_SELECT_ALL);
    return this;
  }

  /**
   * Selects text within the terminal.
   *
   * @param column the column to start selecting from
   * @param row the row to start selecting from
   * @param length the length of the selection
   * @return the component itself
   */
  public Terminal select(int column, int row, int length) {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_SELECT, column, row, length);
    return this;
  }

  /**
   * Selects lines within the terminal.
   *
   * @param start the start line to select
   * @param end the end line to select
   * @return the component itself
   */
  public Terminal selectLines(int start, int end) {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_SELECT_LINES, start, end);
    return this;
  }

  /**
   * Clears the current terminal selection.
   *
   * @return the component itself
   */
  public Terminal clearSelection() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_CLEAR_SELECTION);
    return this;
  }

  /**
   * Gets the terminal selection.
   *
   * @return a pending result for the operation
   */
  public PendingResult<String> getSelectedText() {
    PendingResult<Object> invocation =
        getOriginalElement().callJsFunctionAsync(JS_METHOD_GET_SELECTION);
    return new PendingResult<>(invocation).thenApply(r -> r == null ? null : String.valueOf(r));
  }

  /**
   * Scroll the terminal down a number of lines.
   *
   * @param amount The number of lines to scroll down.
   * @return the component itself
   */
  public Terminal scrollLines(int amount) {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_SCROLL_LINES, amount);
    return this;
  }

  /**
   * Scrolls to a line within the buffer.
   *
   * @param line The 0-based line index to scroll to.
   * @return the component itself
   */
  public Terminal scrollToLine(int line) {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_SCROLL_TO_LINE, line);
    return this;
  }

  /**
   * Scroll the terminal to the top.
   *
   * @return the component itself
   */
  public Terminal scrollToTop() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_SCROLL_TO_TOP);
    return this;
  }

  /**
   * Scroll the terminal to the bottom.
   *
   * @return the component itself
   */
  public Terminal scrollToBottom() {
    getOriginalElement().callJsFunctionAsync(JS_METHOD_SCROLL_TO_BOTTOM);
    return this;
  }

  /**
   * Adds a listener for the terminal data event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TerminalDataEvent> addDataListener(
      EventListener<TerminalDataEvent> listener) {
    return addEventListener(TerminalDataEvent.class, listener);
  }

  /**
   * Alias for {@link #addDataListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TerminalDataEvent> onData(EventListener<TerminalDataEvent> listener) {
    return addDataListener(listener);
  }

  /**
   * Adds a listener for the terminal key event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TerminalKeyEvent> addKeyListener(
      EventListener<TerminalKeyEvent> listener) {
    return addEventListener(TerminalKeyEvent.class, listener);
  }

  /**
   * Alias for {@link #addKeyListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TerminalKeyEvent> onKey(EventListener<TerminalKeyEvent> listener) {
    return addKeyListener(listener);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
