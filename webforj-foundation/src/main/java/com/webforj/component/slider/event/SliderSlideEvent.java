package com.webforj.component.slider.event;

import com.webforj.component.event.ComponentEvent;
import com.webforj.component.slider.Slider;
import java.util.Map;

/**
 * An event which is fired when the user slides the slider.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public class SliderSlideEvent extends ComponentEvent<Slider> {

  public SliderSlideEvent(Slider component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * Gets the current value of the slider.
   *
   * @return the value of the slider
   */
  public int getValue() {
    return (int) this.getEventMap().get("value");
  }

  /**
   * Returns whether the user is still changing the value of the slider.
   *
   * @return {@code true} if the user is still changing the value of the slider, {@code false}
   */
  public boolean isAdjusting() {
    return (boolean) this.getEventMap().get("adjusting");
  }
}
