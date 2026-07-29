package com.webforj.devtools.craftforj.inspector.contribution.state.dialog;

import com.google.auto.service.AutoService;
import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Dialog snap to edge property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DialogSnapToEdgeContribution extends ConcernContribution<Dialog> {

  /**
   * Creates a new DialogSnapToEdgeContribution.
   */
  public DialogSnapToEdgeContribution() {
    super(Dialog.class, "SnapToEdge", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Dialog::isSnapToEdge);
    setSetter((c, v) -> c.setSnapToEdge(Boolean.TRUE.equals(v)));
  }

}
