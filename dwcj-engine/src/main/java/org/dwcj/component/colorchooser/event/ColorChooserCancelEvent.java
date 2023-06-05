package org.dwcj.component.colorchooser.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.event.Event;

/**
 * Cancel Event for the ColorChooser Component.
 */
public class ColorChooserCancelEvent extends Event<AbstractComponent> {

  public ColorChooserCancelEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
