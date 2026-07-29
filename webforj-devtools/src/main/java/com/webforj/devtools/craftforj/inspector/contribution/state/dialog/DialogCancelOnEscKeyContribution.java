package com.webforj.devtools.craftforj.inspector.contribution.state.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Dialog cancel on escape key property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogCancelOnEscKeyContribution extends ConcernContribution<Dialog> {

  /**
   * Creates a new DialogCancelOnEscKeyContribution.
   */
  public DialogCancelOnEscKeyContribution() {
    super(Dialog.class, "CancelOnEscKey", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Dialog::isCancelOnEscKey);
    setSetter((c, v) -> c.setCancelOnEscKey(Boolean.TRUE.equals(v)));
  }

}
