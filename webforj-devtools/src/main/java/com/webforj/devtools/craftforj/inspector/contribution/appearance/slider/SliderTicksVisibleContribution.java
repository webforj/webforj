package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider ticksVisible property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderTicksVisibleContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider ticksVisible contribution.
   */
  public SliderTicksVisibleContribution() {
    super(Slider.class, "TicksVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isTicksVisible);
    setSetter((c, v) -> c.setTicksVisible(Boolean.TRUE.equals(v)));
  }

}
