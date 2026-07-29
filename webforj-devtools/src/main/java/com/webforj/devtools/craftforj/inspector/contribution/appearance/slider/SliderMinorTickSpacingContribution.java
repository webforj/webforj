package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider minorTickSpacing property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderMinorTickSpacingContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider minorTickSpacing contribution.
   */
  public SliderMinorTickSpacingContribution() {
    super(Slider.class, "MinorTickSpacing", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(Slider::getMinorTickSpacing);
    setSetter((c, v) -> c.setMinorTickSpacing(v instanceof Number n ? n.intValue() : 0));
  }

}
