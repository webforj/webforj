package org.dwcj.component.dialog.event;

import java.util.Map;
import org.dwcj.component.dialog.Dialog;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.event.ComponentEvent;

/**
 * Emitted when the dialog is opened.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName("bbj-dialog-opened")
public class DialogOpenEvent extends ComponentEvent<Dialog> {

  /**
   * Creates an open event.
   *
   * @param dialog the control
   * @param eventMap the event map
   */
  public DialogOpenEvent(Dialog dialog, Map<String, Object> eventMap) {
    super(dialog, eventMap);
  }
}
