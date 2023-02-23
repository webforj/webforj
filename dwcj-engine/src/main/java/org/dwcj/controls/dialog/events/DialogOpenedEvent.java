package org.dwcj.controls.dialog.events;

import java.util.Map;

import org.dwcj.controls.dialog.Dialog;
import org.dwcj.webcomponent.annotations.EventName;
import org.dwcj.webcomponent.events.Event;

/**
 * Emitted when the dialog is opened.
 * 
 * @author Hyyan Abo Fakher
 */
@EventName("bbj-opened")
public class DialogOpenedEvent extends Event<Dialog> {

  /**
   * @param control  the control
   * @param eventMap the event map
   */
  public DialogOpenedEvent(Dialog control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}
