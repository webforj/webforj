package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when an element or checkbox is deselected or marked as "unchecked". 
 * This event is triggered when a user interacts with a checkbox, radio button, or any other 
 * form element to remove the "checked" state. It usually indicates that the associated action 
 * or state has been disabled or deactivated.
 * 
 * @see CheckEvent
 * @see ToggleEvent
 */
public class UncheckEvent extends ComponentEvent<Component> {
  public UncheckEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
