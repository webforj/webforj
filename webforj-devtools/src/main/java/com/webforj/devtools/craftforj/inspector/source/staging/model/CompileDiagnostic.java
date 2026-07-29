package com.webforj.devtools.craftforj.inspector.source.staging.model;

import java.util.List;

/**
 * One structured compiler or parser diagnostic for a staged file.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CompileDiagnostic {

  private final String file;
  private final long line;
  private final long column;
  private final String message;
  private final List<String> didYouMean;

  /**
   * Creates a diagnostic.
   *
   * @param file the absolute path of the file the diagnostic belongs to
   * @param line the one based line number, or {@code -1} when unknown
   * @param column the one based column number, or {@code -1} when unknown
   * @param message the compiler message
   * @param didYouMean fully qualified class names matching an unresolved simple name, may be empty
   */
  public CompileDiagnostic(String file, long line, long column, String message,
      List<String> didYouMean) {
    this.file = file;
    this.line = line;
    this.column = column;
    this.message = message;
    this.didYouMean = didYouMean == null ? List.of() : List.copyOf(didYouMean);
  }

  /**
   * Gets the absolute path of the file the diagnostic belongs to.
   *
   * @return the file path
   */
  public String getFile() {
    return file;
  }

  /**
   * Gets the one based line number.
   *
   * @return the line number, or {@code -1} when unknown
   */
  public long getLine() {
    return line;
  }

  /**
   * Gets the one based column number.
   *
   * @return the column number, or {@code -1} when unknown
   */
  public long getColumn() {
    return column;
  }

  /**
   * Gets the compiler message.
   *
   * @return the message
   */
  public String getMessage() {
    return message;
  }

  /**
   * Gets fully qualified class names that match an unresolved simple name in the message.
   *
   * @return the candidate class names, possibly empty
   */
  public List<String> getDidYouMean() {
    return didYouMean;
  }
}
