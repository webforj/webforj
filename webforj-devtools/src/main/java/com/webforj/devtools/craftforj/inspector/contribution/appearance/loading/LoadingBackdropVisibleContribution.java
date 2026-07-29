package com.webforj.devtools.craftforj.inspector.contribution.appearance.loading;

import com.google.auto.service.AutoService;
import com.webforj.component.loading.Loading;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Loading backdrop visible property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class LoadingBackdropVisibleContribution extends ConcernContribution<Loading> {

  /**
   * Creates a new LoadingBackdropVisibleContribution.
   */
  public LoadingBackdropVisibleContribution() {
    super(Loading.class, "BackdropVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Loading::isBackdropVisible);
    setSetter((c, v) -> c.setBackdropVisible(Boolean.TRUE.equals(v)));
  }

}
