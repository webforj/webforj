package com.webforj.devtools.craftforj.inspector.contribution.appearance.table;

import com.google.auto.service.AutoService;
import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Table headerCheckboxSelection property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class TableHeaderCheckboxSelectionContribution extends ConcernContribution<Table<?>> {

  /**
   * Creates a new contribution.
   */
  public TableHeaderCheckboxSelectionContribution() {
    super(Table.class, "HeaderCheckboxSelection", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Table::isHeaderCheckboxSelection);
    setSetter((c, v) -> c.setHeaderCheckboxSelection(Boolean.TRUE.equals(v)));
  }

}
