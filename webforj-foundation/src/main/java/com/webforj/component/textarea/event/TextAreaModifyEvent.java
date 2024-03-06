package com.webforj.component.textarea.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.textarea.TextArea;

public final class TextAreaModifyEvent implements ControlEvent {
  private final TextArea control;

  public TextAreaModifyEvent(TextArea cMultilineEdit) {
    this.control = cMultilineEdit;
  }

  @Override
  public TextArea getControl() {
    return control;
  }

  public String toString() {
    return "Event: MultilineEdit modified";
  }
}
