package com.webforj.devtools.craftforj.inspector.contribution.state.table;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Table selectionMode property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class TableSelectionModeContribution extends EnumConcernContribution<Table<?>> {

  /**
   * Creates a new contribution.
   */
  public TableSelectionModeContribution() {
    super(Table.class, "SelectionMode", FeatureCategory.STATE);
    setGetter(Table::getSelectionMode);
    setSetter((c, v) -> c.setSelectionMode((Table.SelectionMode) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Table.SelectionMode.class;
  }

}
