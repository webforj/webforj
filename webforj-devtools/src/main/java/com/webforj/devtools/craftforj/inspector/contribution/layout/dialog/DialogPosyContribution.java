package com.webforj.devtools.craftforj.inspector.contribution.layout.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Dialog Y position property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogPosyContribution extends ConcernContribution<Dialog> {

  /**
   * Creates a new DialogPosyContribution.
   */
  public DialogPosyContribution() {
    super(Dialog.class, "Posy", FeatureCategory.LAYOUT);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(Dialog::getPosy);
    setSetter((c, v) -> c.setPosy(String.valueOf(v)));
  }

}
