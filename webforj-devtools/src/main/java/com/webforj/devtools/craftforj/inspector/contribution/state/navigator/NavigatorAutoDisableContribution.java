package com.webforj.devtools.craftforj.inspector.contribution.state.navigator;

import com.google.auto.service.AutoService;
import com.webforj.component.navigator.Navigator;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Navigator auto-disable mode.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class NavigatorAutoDisableContribution extends ConcernContribution<Navigator> {

  /**
   * Creates a new NavigatorAutoDisableContribution.
   */
  public NavigatorAutoDisableContribution() {
    super(Navigator.class, "AutoDisable", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Navigator::isAutoDisable);
    setSetter((c, v) -> c.setAutoDisable(Boolean.TRUE.equals(v)));
  }

}
