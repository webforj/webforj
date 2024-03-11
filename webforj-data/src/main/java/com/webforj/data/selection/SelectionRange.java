package com.webforj.data.selection;

import java.util.Objects;

/**
 * Represents a range of selected text within an input component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public class SelectionRange {
  private int startParagraph;
  private int startOffset;
  private int endParagraph;
  private int endOffset;

  /**
   * Constructs a new SelectionRange object with the provided offset values. Assumes the paragraph
   * indices remain the same.
   *
   * @param startOffset the offset within the starting paragraph of the selected text
   * @param endOffset the offset within the ending paragraph of the selected text
   */
  public SelectionRange(int startOffset, int endOffset) {
    this(0, startOffset, 0, endOffset);
  }

  /**
   * Constructs a new SelectionRange object.
   *
   * @param startParagraph the paragraph index of the beginning of the selected text
   * @param startOffset the offset within the starting paragraph of the selected text
   * @param endParagraph the paragraph index of the end of the selected text
   * @param endOffset the offset within the ending paragraph of the selected text
   */
  public SelectionRange(int startParagraph, int startOffset, int endParagraph, int endOffset) {
    this.startParagraph = startParagraph;
    this.startOffset = startOffset;
    this.endParagraph = endParagraph;
    this.endOffset = endOffset;
  }

  /**
   * Retrieves the paragraph index of the beginning of the selected text.
   *
   * @return the paragraph index
   */
  public int getStartParagraph() {
    return startParagraph;
  }

  /**
   * Retrieves the offset within the starting paragraph of the selected text.
   *
   * @return the offset
   */
  public int getStartOffset() {
    return startOffset;
  }

  /**
   * Retrieves the paragraph index of the end of the selected text.
   *
   * @return the paragraph index
   */
  public int getEndParagraph() {
    return endParagraph;
  }

  /**
   * Retrieves the offset within the ending paragraph of the selected text.
   *
   * @return the offset
   */
  public int getEndOffset() {
    return endOffset;
  }

  /**
   * Retrieves the offset within the starting paragraph of the selected text. Alias for
   * {@link #getStartOffset()}.
   *
   * @return the offset
   */
  public int getStart() {
    return getStartOffset();
  }

  /**
   * Retrieves the offset within the ending paragraph of the selected text. Alias for
   * {@link #getEndOffset()}.
   *
   * @return the offset
   */
  public int getEnd() {
    return getEndOffset();
  }

  /**
   * Checks if the selection range is empty.
   *
   * @return true if the selection range is empty, false otherwise
   */
  public boolean isEmpty() {
    return (startParagraph == endParagraph && startOffset == endOffset);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals(Object obj) {
    if (obj instanceof SelectionRange) {
      SelectionRange other = (SelectionRange) obj;
      return startParagraph == other.startParagraph && startOffset == other.startOffset
          && endParagraph == other.endParagraph && endOffset == other.endOffset;
    }

    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode() {
    return Objects.hash(startParagraph, startOffset, endParagraph, endOffset);
  }
}
