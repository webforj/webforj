package org.dwcj.component.dialog.event;

import java.util.Map;
import org.dwcj.component.dialog.Dialog;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.event.ComponentEvent;

/**
 * Emitted when the dialog is closed.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName("dwc-dialog-closed")
public class DialogCloseEvent extends ComponentEvent<Dialog> {

  /**
   * Creates a close event.
   *
   * @param dialog the dialog
   * @param eventMap the event map
   */
  public DialogCloseEvent(Dialog dialog, Map<String, Object> eventMap) {
    super(dialog, eventMap);
  }
}
