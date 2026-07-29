package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider tooltipVisible property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderTooltipVisibleContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider tooltipVisible contribution.
   */
  public SliderTooltipVisibleContribution() {
    super(Slider.class, "TooltipVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isTooltipVisible);
    setSetter((c, v) -> c.setTooltipVisible(Boolean.TRUE.equals(v)));
  }

}
