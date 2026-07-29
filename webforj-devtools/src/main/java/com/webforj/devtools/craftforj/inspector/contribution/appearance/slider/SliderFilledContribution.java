package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider filled property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderFilledContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider filled contribution.
   */
  public SliderFilledContribution() {
    super(Slider.class, "Filled", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isFilled);
    setSetter((c, v) -> c.setFilled(Boolean.TRUE.equals(v)));
  }

}
