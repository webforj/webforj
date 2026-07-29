package com.webforj.devtools.craftforj.inspector.contribution.state.table;

import com.google.auto.service.AutoService;
import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Table multiSelectWithClick property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class TableMultiSelectWithClickContribution extends ConcernContribution<Table<?>> {

  /**
   * Creates a new contribution.
   */
  public TableMultiSelectWithClickContribution() {
    super(Table.class, "MultiSelectWithClick", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Table::isMultiSelectWithClick);
    setSetter((c, v) -> c.setMultiSelectWithClick(Boolean.TRUE.equals(v)));
  }

}
