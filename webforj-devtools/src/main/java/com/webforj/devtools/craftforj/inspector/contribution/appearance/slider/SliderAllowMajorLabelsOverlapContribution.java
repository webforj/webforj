package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider allowMajorLabelsOverlap property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderAllowMajorLabelsOverlapContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider allowMajorLabelsOverlap contribution.
   */
  public SliderAllowMajorLabelsOverlapContribution() {
    super(Slider.class, "AllowMajorLabelsOverlap", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isAllowMajorLabelsOverlap);
    setSetter((c, v) -> c.setAllowMajorLabelsOverlap(Boolean.TRUE.equals(v)));
  }

}
