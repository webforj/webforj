package com.webforj.component.event;

import com.webforj.component.Component;
import java.util.Map;

/**
 * This event is fired when an element or checkbox is selected or marked as "checked".
 *
 * <p>
 * This event is triggered when a user interacts with a checkbox, radio button, or any other form
 * element with a "checked" state. It typically indicates that the associated action or state has
 * been enabled or activated. This event is the opposite of an Uncheck event.
 * </p>
 *
 * @see UncheckEvent
 * @see ToggleEvent
 */
public class CheckEvent extends ComponentEvent<Component> {
  public CheckEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
