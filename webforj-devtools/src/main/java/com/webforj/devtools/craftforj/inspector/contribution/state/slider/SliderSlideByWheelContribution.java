package com.webforj.devtools.craftforj.inspector.contribution.state.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider slideByWheel property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderSlideByWheelContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider slideByWheel contribution.
   */
  public SliderSlideByWheelContribution() {
    super(Slider.class, "SlideByWheel", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isSlideByWheel);
    setSetter((c, v) -> c.setSlideByWheel(Boolean.TRUE.equals(v)));
  }

}
