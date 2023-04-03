package org.dwcj.component.dialog.event;

import java.util.Map;

import org.dwcj.component.dialog.Dialog;
import org.dwcj.component.webcomponent.annotations.EventName;
import org.dwcj.component.webcomponent.events.Event;

/**
 * Emitted when the dialog is closed.
 * 
 * @author Hyyan Abo Fakher
 */
@EventName("bbj-closed")
public class DialogCloseEvent extends Event<Dialog> {

  /**
   * @param control  the control
   * @param eventMap the event map
   */
  public DialogCloseEvent(Dialog control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}