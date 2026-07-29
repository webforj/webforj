package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMaxFileSize;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMaxFileSize concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMaxFileSizeContribution extends ConcernContribution<HasMaxFileSize<?>> {

  /**
   * Creates the HasMaxFileSize contribution.
   */
  public HasMaxFileSizeContribution() {
    super(HasMaxFileSize.class, "MaxFileSize", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(HasMaxFileSize::getMaxFileSize);
    setSetter((c, v) -> c.setMaxFileSize((Number) v));
  }
}
