package com.webforj.devtools.craftforj.inspector.contribution.layout.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Dialog auto width mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogAutoWidthContribution extends ConcernContribution<Dialog> {

  /**
   * Creates a new DialogAutoWidthContribution.
   */
  public DialogAutoWidthContribution() {
    super(Dialog.class, "AutoWidth", FeatureCategory.LAYOUT);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Dialog::isAutoWidth);
    setSetter((c, v) -> c.setAutoWidth(Boolean.TRUE.equals(v)));
  }
}
