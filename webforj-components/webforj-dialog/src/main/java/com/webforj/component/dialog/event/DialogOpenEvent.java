package com.webforj.component.dialog.event;

import com.webforj.component.dialog.Dialog;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

/**
 * Emitted when the dialog is opened.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName("dwc-dialog-opened")
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
