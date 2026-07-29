package com.webforj.devtools.craftforj.inspector.contribution.layout.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the Slider orientation property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderOrientationContribution extends EnumConcernContribution<Slider> {

  /**
   * Creates the Slider orientation contribution.
   */
  public SliderOrientationContribution() {
    super(Slider.class, "Orientation", FeatureCategory.LAYOUT);
    setGetter(Slider::getOrientation);
    setSetter((c, v) -> c.setOrientation((Slider.Orientation) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Slider.Orientation.class;
  }

}
