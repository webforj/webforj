package org.dwcj.component.dialog.event;

import java.util.Map;
import org.dwcj.component.dialog.Dialog;
import org.dwcj.component.event.Event;
import org.dwcj.component.webcomponent.annotation.EventName;

/**
 * Emitted when the dialog is opened.
 *
 * @author Hyyan Abo Fakher
 */
@EventName("bbj-opened")
public class DialogOpenEvent extends Event<Dialog> {

  /**
   * Creates a new event.
   *
   * @param control the control
   * @param eventMap the event map
   */
  public DialogOpenEvent(Dialog control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}
