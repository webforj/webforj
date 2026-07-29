package com.webforj.devtools.craftforj.inspector.contribution.state.table;

import com.google.auto.service.AutoService;
import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Table deselection property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class TableDeselectionContribution extends ConcernContribution<Table<?>> {

  /**
   * Creates a new contribution.
   */
  public TableDeselectionContribution() {
    super(Table.class, "Deselection", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Table::isDeselection);
    setSetter((c, v) -> c.setDeselection(Boolean.TRUE.equals(v)));
  }

}
