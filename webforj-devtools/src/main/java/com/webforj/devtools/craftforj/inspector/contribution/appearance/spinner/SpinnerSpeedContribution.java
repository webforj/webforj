package com.webforj.devtools.craftforj.inspector.contribution.appearance.spinner;

import com.google.auto.service.AutoService;
import com.webforj.component.spinner.Spinner;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Spinner speed property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SpinnerSpeedContribution extends ConcernContribution<Spinner> {

  /**
   * Creates a new SpinnerSpeedContribution.
   */
  public SpinnerSpeedContribution() {
    super(Spinner.class, "Speed", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(Spinner::getSpeed);
    setSetter((c, v) -> c.setSpeed(((Number) v).intValue()));
  }

}
