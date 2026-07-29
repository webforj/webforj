package com.webforj.devtools.craftforj.inspector.contribution.appearance.table;

import com.google.auto.service.AutoService;
import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Table groupHeaderHeight property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TableGroupHeaderHeightContribution extends ConcernContribution<Table<?>> {

  /**
   * Creates a new contribution.
   */
  public TableGroupHeaderHeightContribution() {
    super(Table.class, "GroupHeaderHeight", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::decimal);
    setGetter(Table::getGroupHeaderHeight);
    setSetter((c, v) -> c.setGroupHeaderHeight(((Number) v).doubleValue()));
  }

}
