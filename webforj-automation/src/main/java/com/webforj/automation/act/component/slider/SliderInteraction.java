package com.webforj.automation.act.component.slider;

import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.slider.Slider;
import com.webforj.component.slider.event.SliderSlideEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Slides sliders within their range.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class SliderInteraction {

  private SliderInteraction() {}

  /**
   * Returns the interaction that slides a slider to the given value, the way the user drags the
   * handle and lets go. The slider takes the value, then reports it.
   *
   * <pre>{@code
   * UserInteraction.perform(slider, SliderInteraction.slide(42))
   * }</pre>
   *
   * @param value the value the user lets go at
   *
   * @return the interaction to perform on the slider
   * @throws InteractionRejectedException when performed, if the value is outside the range of the
   *         slider
   */
  public static Consumer<Slider> slide(int value) {
    return slider -> {
      if (value < slider.getMin() || value > slider.getMax()) {
        throw new InteractionRejectedException(slider, "ranges from " + slider.getMin() + " to "
            + slider.getMax() + " and a user cannot slide it to " + value);
      }

      slider.setValue(value);

      Map<String, Object> data = new HashMap<>();
      data.put("value", value);
      data.put("adjusting", false);
      UserInteraction.fireEvent(new SliderSlideEvent(slider, data));
    };
  }
}
