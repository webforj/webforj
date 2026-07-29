package com.webforj.devtools.craftforj.inspector.contribution.appearance.upload;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Upload preset.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class UploadPresetContribution extends EnumConcernContribution<Upload> {

  /**
   * Creates a new UploadPresetContribution.
   */
  public UploadPresetContribution() {
    super(Upload.class, "Preset", FeatureCategory.APPEARANCE);
    setGetter(Upload::getPreset);
    setSetter((c, v) -> c.setPreset((Upload.Preset) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Upload.Preset.class;
  }

}
