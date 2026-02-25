package com.webforj.component.accordion.event;

import com.webforj.component.accordion.AccordionPanel;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

/**
 * Emitted after the accordion panel has fully closed.
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@EventName(value = "dwc-accordion-panel-closed")
@EventOptions(filter = "event.target.isSameNode(component)")
public final class AccordionPanelCloseEvent extends ComponentEvent<AccordionPanel> {

  /**
   * Creates a close event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AccordionPanelCloseEvent(AccordionPanel target, Map<String, Object> detail) {
    super(target, detail);
  }
}
