package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Upload auto upload mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class UploadAutoUploadContribution extends EnumConcernContribution<Upload> {

  /**
   * Creates a new UploadAutoUploadContribution.
   */
  public UploadAutoUploadContribution() {
    super(Upload.class, "AutoUpload", FeatureCategory.STATE);
    setGetter(Upload::getAutoUpload);
    setSetter((c, v) -> c.setAutoUpload((Upload.AutoUpload) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Upload.AutoUpload.class;
  }

}
