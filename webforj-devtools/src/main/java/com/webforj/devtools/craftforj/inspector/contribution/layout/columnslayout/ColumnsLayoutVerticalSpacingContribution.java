package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for ColumnsLayout vertical spacing property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ColumnsLayoutVerticalSpacingContribution extends ConcernContribution<ColumnsLayout> {

  /** Creates the ColumnsLayout vertical spacing contribution. */
  public ColumnsLayoutVerticalSpacingContribution() {
    super(ColumnsLayout.class, "VerticalSpacing", FeatureCategory.LAYOUT);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(ColumnsLayout::getVerticalSpacing);
    setSetter((c, v) -> c.setVerticalSpacing(v == null ? null : String.valueOf(v)));
  }
}
