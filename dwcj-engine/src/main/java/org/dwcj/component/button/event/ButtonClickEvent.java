package org.dwcj.component.button.event;

import java.util.Map;
import org.dwcj.component.Component;
import org.dwcj.component.button.DwcButton;
import org.dwcj.component.event.ComponentEvent;

/**
 * An event which is fired when the user clicks on a button.
 *
 * <p>
 * This event is essential for capturing user interactions with buttons on a web page, such as
 * submitting forms, triggering actions, or navigating to different sections of the site.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public class ButtonClickEvent extends ComponentEvent<DwcButton<?>> {

  /**
   * Creates a new event.
   *
   * @param abstractDwcComponent the component
   * @param payload the event map
   */
  public ButtonClickEvent(DwcButton<?> abstractDwcComponent, Map<String, Object> payload) {
    super(abstractDwcComponent, payload);
  }

  /**
   * Get the X location of the mouse within the button when the event occurred.
   *
   * @return the X location of the mouse within the button.
   */
  public double getX() {
    return (double) this.getEventMap().get("x");
  }

  /**
   * Get the Y location of the mouse within the button when the event occurred.
   *
   * @return the Y location of the mouse within the button.
   */
  public double getY() {
    return (double) this.getEventMap().get("y");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DwcButton<?> getComponent() {
    return (DwcButton<?>) super.getComponent();
  }
}

