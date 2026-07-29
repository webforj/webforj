package com.webforj.devtools.craftforj.inspector.contribution.state.progressbar;

import com.google.auto.service.AutoService;
import com.webforj.component.progressbar.ProgressBar;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the ProgressBar indeterminate property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ProgressBarIndeterminateContribution extends ConcernContribution<ProgressBar> {

  /**
   * Creates the ProgressBar indeterminate contribution.
   */
  public ProgressBarIndeterminateContribution() {
    super(ProgressBar.class, "Indeterminate", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(ProgressBar::isIndeterminate);
    setSetter((c, v) -> c.setIndeterminate(Boolean.TRUE.equals(v)));
  }

}
