package com.webforj.component.dialog.event;

import com.webforj.component.dialog.Dialog;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

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
