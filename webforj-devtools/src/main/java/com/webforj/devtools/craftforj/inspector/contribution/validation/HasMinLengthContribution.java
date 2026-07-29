package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMinLength;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMinLength concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMinLengthContribution extends ConcernContribution<HasMinLength<?>> {

  /**
   * Creates the HasMinLength contribution.
   */
  public HasMinLengthContribution() {
    super(HasMinLength.class, "MinLength", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(HasMinLength::getMinLength);
    setSetter((c, v) -> c.setMinLength(((Number) v).intValue()));
  }
}
