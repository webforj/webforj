package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Upload capture mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class UploadCaptureContribution extends EnumConcernContribution<Upload> {

  /**
   * Creates a new UploadCaptureContribution.
   */
  public UploadCaptureContribution() {
    super(Upload.class, "Capture", FeatureCategory.STATE);
    setGetter(Upload::getCapture);
    setSetter((c, v) -> c.setCapture((Upload.Capture) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Upload.Capture.class;
  }

}
