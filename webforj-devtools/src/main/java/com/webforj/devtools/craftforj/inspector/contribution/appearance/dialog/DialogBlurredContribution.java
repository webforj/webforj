package com.webforj.devtools.craftforj.inspector.contribution.appearance.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Dialog blurred backdrop property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogBlurredContribution extends ConcernContribution<Dialog> {

  /**
   * Creates a new DialogBlurredContribution.
   */
  public DialogBlurredContribution() {
    super(Dialog.class, "Blurred", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Dialog::isBlurred);
    setSetter((c, v) -> c.setBlurred(Boolean.TRUE.equals(v)));
  }

}
