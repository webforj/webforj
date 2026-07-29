package com.webforj.devtools.craftforj.inspector.contribution.appearance.table;

import com.google.auto.service.AutoService;
import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Table striped property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class TableStripedContribution extends ConcernContribution<Table<?>> {

  /**
   * Creates a new contribution.
   */
  public TableStripedContribution() {
    super(Table.class, "Striped", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Table::isStriped);
    setSetter((c, v) -> c.setStriped(Boolean.TRUE.equals(v)));
  }

}
