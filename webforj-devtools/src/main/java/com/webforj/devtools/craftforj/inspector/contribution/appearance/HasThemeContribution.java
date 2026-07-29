package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasTheme;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the HasTheme concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasThemeContribution extends EnumConcernContribution<HasTheme<?, ?>> {

  /**
   * Creates the HasTheme contribution.
   */
  @SuppressWarnings("rawtypes")
  public HasThemeContribution() {
    super(HasTheme.class, "Theme", FeatureCategory.APPEARANCE);
    setGetter(HasTheme::getTheme);
    setSetter((c, v) -> ((HasTheme) c).setTheme(v));
  }
}
