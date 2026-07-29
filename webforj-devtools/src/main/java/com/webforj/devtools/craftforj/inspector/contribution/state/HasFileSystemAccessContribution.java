package com.webforj.devtools.craftforj.inspector.contribution.state;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasFileSystemAccess;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasFileSystemAccess concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasFileSystemAccessContribution extends ConcernContribution<HasFileSystemAccess<?>> {

  /**
   * Creates the HasFileSystemAccess contribution.
   */
  public HasFileSystemAccessContribution() {
    super(HasFileSystemAccess.class, "FileSystemAccess", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasFileSystemAccess::isFileSystemAccess);
    setSetter((c, v) -> c.setFileSystemAccess(Boolean.TRUE.equals(v)));
  }
}
