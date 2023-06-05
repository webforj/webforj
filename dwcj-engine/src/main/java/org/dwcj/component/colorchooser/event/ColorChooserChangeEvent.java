package org.dwcj.component.colorchooser.event;

import java.awt.Color;
import java.util.Map;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.event.Event;

/**
 * Change Event for the ColorChooser Component.
 */
public class ColorChooserChangeEvent extends Event<AbstractComponent> {
  public ColorChooserChangeEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }

  public Color getColor() {
    return (Color) this.getEventMap().get("color");
  }
}
