package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.component.layout.columnslayout.ColumnsLayout.Alignment;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for ColumnsLayout vertical alignment property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ColumnsLayoutVerticalAlignmentContribution
    extends EnumConcernContribution<ColumnsLayout> {

  /** Creates the ColumnsLayout vertical alignment contribution. */
  public ColumnsLayoutVerticalAlignmentContribution() {
    super(ColumnsLayout.class, "VerticalAlignment", FeatureCategory.LAYOUT);
    setGetter(ColumnsLayout::getVerticalAlignment);
    setSetter((c, v) -> c.setVerticalAlignment((Alignment) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Alignment.class;
  }
}
