package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMask;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMask concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMaskContribution extends ConcernContribution<HasMask<?>> {

  /**
   * Creates the HasMask contribution.
   */
  public HasMaskContribution() {
    super(HasMask.class, "Mask", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(HasMask::getMask);
    setSetter((c, v) -> c.setMask(String.valueOf(v)));
  }
}
