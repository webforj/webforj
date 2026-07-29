package com.webforj.devtools.craftforj.inspector.contribution.layout.navigator;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.navigator.Navigator;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Navigator layout.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class NavigatorLayoutContribution extends EnumConcernContribution<Navigator> {

  /**
   * Creates a new NavigatorLayoutContribution.
   */
  public NavigatorLayoutContribution() {
    super(Navigator.class, "Layout", FeatureCategory.LAYOUT);
    setGetter(Navigator::getLayout);
    setSetter((c, v) -> c.setLayout((Navigator.Layout) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Navigator.Layout.class;
  }

}
