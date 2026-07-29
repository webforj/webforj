package com.webforj.devtools.craftforj.inspector.contribution.state.spinner;

import com.google.auto.service.AutoService;
import com.webforj.component.spinner.Spinner;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Spinner paused property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SpinnerPausedContribution extends ConcernContribution<Spinner> {

  /**
   * Creates a new SpinnerPausedContribution.
   */
  public SpinnerPausedContribution() {
    super(Spinner.class, "Paused", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Spinner::isPaused);
    setSetter((c, v) -> c.setPaused(Boolean.TRUE.equals(v)));
  }

}
