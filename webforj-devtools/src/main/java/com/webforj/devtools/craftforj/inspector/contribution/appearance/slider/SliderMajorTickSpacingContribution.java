package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider majorTickSpacing property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderMajorTickSpacingContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider majorTickSpacing contribution.
   */
  public SliderMajorTickSpacingContribution() {
    super(Slider.class, "MajorTickSpacing", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(Slider::getMajorTickSpacing);
    setSetter((c, v) -> c.setMajorTickSpacing(v instanceof Number n ? n.intValue() : 0));
  }

}
