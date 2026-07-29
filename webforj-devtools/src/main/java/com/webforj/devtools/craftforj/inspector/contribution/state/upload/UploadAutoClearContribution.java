package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Upload auto clear mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class UploadAutoClearContribution extends EnumConcernContribution<Upload> {

  /**
   * Creates a new UploadAutoClearContribution.
   */
  public UploadAutoClearContribution() {
    super(Upload.class, "AutoClear", FeatureCategory.STATE);
    setGetter(Upload::getAutoClear);
    setSetter((c, v) -> c.setAutoClear((Upload.AutoClear) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Upload.AutoClear.class;
  }

}
