package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasPattern;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasPattern concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasPatternContribution extends ConcernContribution<HasPattern<?>> {

  /**
   * Creates the HasPattern contribution.
   */
  public HasPatternContribution() {
    super(HasPattern.class, "Pattern", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(HasPattern::getPattern);
    setSetter((c, v) -> c.setPattern(String.valueOf(v)));
  }
}
