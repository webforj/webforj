package com.webforj.devtools.craftforj.inspector.contribution.appearance.table;

import com.google.auto.service.AutoService;
import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Table headerHeight property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class TableHeaderHeightContribution extends ConcernContribution<Table<?>> {

  /**
   * Creates a new contribution.
   */
  public TableHeaderHeightContribution() {
    super(Table.class, "HeaderHeight", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::decimal);
    setGetter(Table::getHeaderHeight);
    setSetter((c, v) -> c.setHeaderHeight(((Number) v).doubleValue()));
  }

}
