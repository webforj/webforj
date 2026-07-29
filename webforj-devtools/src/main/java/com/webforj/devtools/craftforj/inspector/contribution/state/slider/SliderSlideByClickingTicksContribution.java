package com.webforj.devtools.craftforj.inspector.contribution.state.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider slideByClickingTicks property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderSlideByClickingTicksContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider slideByClickingTicks contribution.
   */
  public SliderSlideByClickingTicksContribution() {
    super(Slider.class, "SlideByClickingTicks", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isSlideByClickingTicks);
    setSetter((c, v) -> c.setSlideByClickingTicks(Boolean.TRUE.equals(v)));
  }

}
