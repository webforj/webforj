package com.webforj.component.accordion.event;

import com.webforj.component.accordion.AccordionPanel;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

/**
 * Emitted before the accordion panel state changes. The event detail contains the new opened state.
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@EventName(value = "dwc-accordion-panel-toggled")
@EventOptions(filter = "event.target.isSameNode(component)",
    data = {@EventData(key = "isOpened", exp = "event.detail")})
public final class AccordionPanelToggleEvent extends ComponentEvent<AccordionPanel> {

  /**
   * Creates a toggle event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AccordionPanelToggleEvent(AccordionPanel target, Map<String, Object> detail) {
    super(target, detail);
  }

  /**
   * Returns the new opened state that the panel is transitioning to.
   *
   * @return true if the panel is opening, false if closing
   */
  public boolean isOpened() {
    return (boolean) getData().get("isOpened");
  }
}
