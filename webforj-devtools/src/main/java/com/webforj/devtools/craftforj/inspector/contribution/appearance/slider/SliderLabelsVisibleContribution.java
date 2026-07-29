package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import com.google.auto.service.AutoService;
import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Slider labelsVisible property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SliderLabelsVisibleContribution extends ConcernContribution<Slider> {

  /**
   * Creates the Slider labelsVisible contribution.
   */
  public SliderLabelsVisibleContribution() {
    super(Slider.class, "LabelsVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Slider::isLabelsVisible);
    setSetter((c, v) -> c.setLabelsVisible(Boolean.TRUE.equals(v)));
  }

}
