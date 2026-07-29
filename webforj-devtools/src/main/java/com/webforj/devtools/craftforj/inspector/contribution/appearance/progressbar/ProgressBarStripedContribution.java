package com.webforj.devtools.craftforj.inspector.contribution.appearance.progressbar;

import com.google.auto.service.AutoService;
import com.webforj.component.progressbar.ProgressBar;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the ProgressBar striped property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ProgressBarStripedContribution extends ConcernContribution<ProgressBar> {

  /**
   * Creates the ProgressBar striped contribution.
   */
  public ProgressBarStripedContribution() {
    super(ProgressBar.class, "Striped", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(ProgressBar::isStriped);
    setSetter((c, v) -> c.setStriped(Boolean.TRUE.equals(v)));
  }

}
