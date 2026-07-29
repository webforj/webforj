package com.webforj.devtools.craftforj.inspector.contribution.layout.progressbar;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.progressbar.ProgressBar;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the ProgressBar orientation property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ProgressBarOrientationContribution extends EnumConcernContribution<ProgressBar> {

  /**
   * Creates the ProgressBar orientation contribution.
   */
  public ProgressBarOrientationContribution() {
    super(ProgressBar.class, "Orientation", FeatureCategory.LAYOUT);
    setGetter(ProgressBar::getOrientation);
    setSetter((c, v) -> c.setOrientation((ProgressBar.Orientation) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return ProgressBar.Orientation.class;
  }

}
