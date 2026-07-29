package com.webforj.devtools.craftforj.inspector.contribution.appearance.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Dialog backdrop property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogBackdropContribution extends ConcernContribution<Dialog> {

  /**
   * Creates a new DialogBackdropContribution.
   */
  public DialogBackdropContribution() {
    super(Dialog.class, "Backdrop", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Dialog::isBackdrop);
    setSetter((c, v) -> c.setBackdrop(Boolean.TRUE.equals(v)));
  }

}
