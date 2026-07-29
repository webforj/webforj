package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider tooltipVisibleOnSlideOnly property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderTooltipVisibleOnSlideOnlyContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider tooltipVisibleOnSlideOnly contribution.
   */
  public SliderTooltipVisibleOnSlideOnlyContribution() {
    super(Slider.class, "TooltipVisibleOnSlideOnly", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isTooltipVisibleOnSlideOnly);
    setSetter((c, v) -> c.setTooltipVisibleOnSlideOnly(Boolean.TRUE.equals(v)));
  }

}
