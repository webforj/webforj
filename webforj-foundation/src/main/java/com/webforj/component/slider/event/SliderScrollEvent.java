package com.webforj.component.slider.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.slider.Slider;

public class SliderScrollEvent implements ControlEvent {

  private final Slider control;

  public SliderScrollEvent(Slider slider) {
    this.control = slider;
  }

  @Override
  public Slider getControl() {
    return control;
  }
}
