package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import com.google.auto.service.AutoService;
import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Upload all files filter toggle.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class UploadAllFilesFilterEnabledContribution extends ConcernContribution<Upload> {

  /**
   * Creates a new UploadAllFilesFilterEnabledContribution.
   */
  public UploadAllFilesFilterEnabledContribution() {
    super(Upload.class, "AllFilesFilterEnabled", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Upload::isAllFilesFilterEnabled);
    setSetter((c, v) -> c.setAllFilesFilterEnabled(Boolean.TRUE.equals(v)));
  }
}
