package org.dwcj.controls.dialog.events;

import java.util.Map;

import org.dwcj.controls.dialog.Dialog;
import org.dwcj.webcomponent.annotations.EventName;
import org.dwcj.webcomponent.events.Event;

/**
 * Emitted when the dialog is closed.
 * 
 * @author Hyyan Abo Fakher
 */
@EventName("bbj-closed")
public class DialogClosedEvent extends Event<Dialog> {

  /**
   * @param control  the control
   * @param eventMap the event map
   */
  public DialogClosedEvent(Dialog control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}