package org.dwcj.component.field;

/** Contains information about the selected text. */
public class FieldselectionInfo {
  private final Integer begin;
  private final Integer end;
  private final String selectedText;

  /** Constructor which takes the begin, end and the selected text. */
  public FieldselectionInfo(Integer begin, Integer end, String text) {
    this.begin = begin;
    this.end = end;
    this.selectedText = text;
  }

  public Integer getBegin() {
    return begin;
  }

  public Integer getEnd() {
    return end;
  }

  public String getSelectedText() {
    return selectedText;
  }
    
}
