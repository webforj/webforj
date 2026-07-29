package com.webforj.devtools.craftforj.inspector.contribution.appearance.progressbar;

import com.google.auto.service.AutoService;
import com.webforj.component.progressbar.ProgressBar;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the ProgressBar textVisible property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ProgressBarTextVisibleContribution extends ConcernContribution<ProgressBar> {

  /**
   * Creates the ProgressBar textVisible contribution.
   */
  public ProgressBarTextVisibleContribution() {
    super(ProgressBar.class, "TextVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(ProgressBar::isTextVisible);
    setSetter((c, v) -> c.setTextVisible(Boolean.TRUE.equals(v)));
  }

}
