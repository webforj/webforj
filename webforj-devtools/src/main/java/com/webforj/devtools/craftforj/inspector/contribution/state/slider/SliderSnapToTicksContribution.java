package com.webforj.devtools.craftforj.inspector.contribution.state.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider snapToTicks property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderSnapToTicksContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider snapToTicks contribution.
   */
  public SliderSnapToTicksContribution() {
    super(Slider.class, "SnapToTicks", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isSnapToTicks);
    setSetter((c, v) -> c.setSnapToTicks(Boolean.TRUE.equals(v)));
  }

}
