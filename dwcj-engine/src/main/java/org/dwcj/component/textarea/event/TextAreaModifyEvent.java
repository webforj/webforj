package org.dwcj.component.textarea.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.textarea.TextArea;

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
