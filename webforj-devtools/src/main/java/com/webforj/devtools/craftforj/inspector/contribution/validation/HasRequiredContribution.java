package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasRequired;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasRequired concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasRequiredContribution extends ConcernContribution<HasRequired<?>> {

  /**
   * Creates the HasRequired contribution.
   */
  public HasRequiredContribution() {
    super(HasRequired.class, "Required", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasRequired::isRequired);
    setSetter((c, v) -> c.setRequired(Boolean.TRUE.equals(v)));
  }
}
