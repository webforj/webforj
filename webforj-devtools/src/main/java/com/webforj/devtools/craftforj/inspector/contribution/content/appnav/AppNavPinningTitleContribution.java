package com.webforj.devtools.craftforj.inspector.contribution.content.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNav pinned section title.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavPinningTitleContribution extends ConcernContribution<AppNav> {

  /**
   * Creates a new AppNavPinningTitleContribution.
   */
  public AppNavPinningTitleContribution() {
    super(AppNav.class, "PinningTitle", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(c -> c.getPinning().getTitle());
    setSetter((c, v) -> c.getPinning().setTitle(String.valueOf(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setTitle";
  }

  @Override
  public String getSourceAccessor() {
    return "getPinning";
  }
}
