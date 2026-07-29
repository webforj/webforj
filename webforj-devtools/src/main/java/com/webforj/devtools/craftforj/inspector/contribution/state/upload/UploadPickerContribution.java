package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Upload picker mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class UploadPickerContribution extends EnumConcernContribution<Upload> {

  /**
   * Creates a new UploadPickerContribution.
   */
  public UploadPickerContribution() {
    super(Upload.class, "Picker", FeatureCategory.STATE);
    setGetter(Upload::getPicker);
    setSetter((c, v) -> c.setPicker((Upload.Picker) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Upload.Picker.class;
  }

}
