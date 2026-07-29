package com.webforj.devtools.craftforj.inspector.contribution.appearance.progressbar;

import com.google.auto.service.AutoService;
import com.webforj.component.progressbar.ProgressBar;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the ProgressBar animated property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ProgressBarAnimatedContribution extends ConcernContribution<ProgressBar> {

  /**
   * Creates the ProgressBar animated contribution.
   */
  public ProgressBarAnimatedContribution() {
    super(ProgressBar.class, "Animated", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(ProgressBar::isAnimated);
    setSetter((c, v) -> c.setAnimated(Boolean.TRUE.equals(v)));
  }

}
