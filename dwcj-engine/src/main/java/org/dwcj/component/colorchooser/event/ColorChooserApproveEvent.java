package org.dwcj.component.colorchooser.event;

import org.dwcj.component.AbstractComponent;
import org.dwcj.component.event.Event;

import java.awt.*;
import java.util.Map;

public class ColorChooserApproveEvent extends Event<AbstractComponent> {

  public ColorChooserApproveEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }

  public Color getColor() {
    return (Color) this.getEventMap().get("color");
  }

}
