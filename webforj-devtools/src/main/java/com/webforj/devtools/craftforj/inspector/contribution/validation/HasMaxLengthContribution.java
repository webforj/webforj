package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMaxLength;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMaxLength concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMaxLengthContribution extends ConcernContribution<HasMaxLength<?>> {

  /**
   * Creates the HasMaxLength contribution.
   */
  public HasMaxLengthContribution() {
    super(HasMaxLength.class, "MaxLength", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(HasMaxLength::getMaxLength);
    setSetter((c, v) -> c.setMaxLength(((Number) v).intValue()));
  }
}
