package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Upload selection mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class UploadSelectionModeContribution extends EnumConcernContribution<Upload> {

  /**
   * Creates a new UploadSelectionModeContribution.
   */
  public UploadSelectionModeContribution() {
    super(Upload.class, "SelectionMode", FeatureCategory.STATE);
    setGetter(Upload::getSelectionMode);
    setSetter((c, v) -> c.setSelectionMode((Upload.SelectionMode) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Upload.SelectionMode.class;
  }

}
