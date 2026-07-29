package com.webforj.devtools.craftforj.inspector.contribution.content.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNav search empty message.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavSearchEmptyMessageContribution extends ConcernContribution<AppNav> {

  /**
   * Creates a new AppNavSearchEmptyMessageContribution.
   */
  public AppNavSearchEmptyMessageContribution() {
    super(AppNav.class, "SearchEmptyMessage", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(c -> c.getSearch().getEmptyMessage());
    setSetter((c, v) -> c.getSearch().setEmptyMessage(String.valueOf(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setEmptyMessage";
  }

  @Override
  public String getSourceAccessor() {
    return "getSearch";
  }
}
