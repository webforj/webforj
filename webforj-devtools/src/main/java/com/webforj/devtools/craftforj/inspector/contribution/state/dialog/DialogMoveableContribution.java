package com.webforj.devtools.craftforj.inspector.contribution.state.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Dialog moveable property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogMoveableContribution extends ConcernContribution<Dialog> {

  /**
   * Creates a new DialogMoveableContribution.
   */
  public DialogMoveableContribution() {
    super(Dialog.class, "Moveable", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Dialog::isMoveable);
    setSetter((c, v) -> c.setMoveable(Boolean.TRUE.equals(v)));
  }

}
