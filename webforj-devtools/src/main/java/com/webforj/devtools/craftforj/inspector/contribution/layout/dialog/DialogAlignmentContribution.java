package com.webforj.devtools.craftforj.inspector.contribution.layout.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Dialog alignment property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogAlignmentContribution extends EnumConcernContribution<Dialog> {

  /**
   * Creates a new DialogAlignmentContribution.
   */
  public DialogAlignmentContribution() {
    super(Dialog.class, "Alignment", FeatureCategory.LAYOUT);
    setGetter(Dialog::getAlignment);
    setSetter((c, v) -> c.setAlignment((Dialog.Alignment) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Dialog.Alignment.class;
  }

}
