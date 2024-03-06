package com.webforj.component.button.event;

import com.webforj.component.Component;
import com.webforj.component.button.DwcButton;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

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

