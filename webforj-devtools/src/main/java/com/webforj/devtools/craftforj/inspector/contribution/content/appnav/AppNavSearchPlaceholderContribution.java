package com.webforj.devtools.craftforj.inspector.contribution.content.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNav search field placeholder.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavSearchPlaceholderContribution extends ConcernContribution<AppNav> {

  /**
   * Creates a new AppNavSearchPlaceholderContribution.
   */
  public AppNavSearchPlaceholderContribution() {
    super(AppNav.class, "SearchPlaceholder", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(c -> c.getSearch().getPlaceholder());
    setSetter((c, v) -> c.getSearch().setPlaceholder(String.valueOf(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setPlaceholder";
  }

  @Override
  public String getSourceAccessor() {
    return "getSearch";
  }
}
