package org.dwcj.component.colorchooser.event;

import org.dwcj.component.AbstractComponent;
import org.dwcj.component.event.Event;

import java.util.Map;

public class ColorChooserCancelEvent extends Event<AbstractComponent> {

  public ColorChooserCancelEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
