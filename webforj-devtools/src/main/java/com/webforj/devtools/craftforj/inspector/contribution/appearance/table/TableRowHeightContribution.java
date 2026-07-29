package com.webforj.devtools.craftforj.inspector.contribution.appearance.table;

import com.google.auto.service.AutoService;
import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Table rowHeight property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class TableRowHeightContribution extends ConcernContribution<Table<?>> {

  /**
   * Creates a new contribution.
   */
  public TableRowHeightContribution() {
    super(Table.class, "RowHeight", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::decimal);
    setGetter(Table::getRowHeight);
    setSetter((c, v) -> c.setRowHeight(((Number) v).doubleValue()));
  }

}
