package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMaxFiles;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMaxFiles concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMaxFilesContribution extends ConcernContribution<HasMaxFiles<?>> {

  /**
   * Creates the HasMaxFiles contribution.
   */
  public HasMaxFilesContribution() {
    super(HasMaxFiles.class, "MaxFiles", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(HasMaxFiles::getMaxFiles);
    setSetter((c, v) -> c.setMaxFiles((Number) v));
  }
}
