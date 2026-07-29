package com.webforj.devtools.craftforj.inspector.contribution.state.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Dialog cancel on outside click property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogCancelOnOutsideClickContribution extends ConcernContribution<Dialog> {

  /**
   * Creates a new DialogCancelOnOutsideClickContribution.
   */
  public DialogCancelOnOutsideClickContribution() {
    super(Dialog.class, "CancelOnOutsideClick", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Dialog::isCancelOnOutsideClick);
    setSetter((c, v) -> c.setCancelOnOutsideClick(Boolean.TRUE.equals(v)));
  }

}
