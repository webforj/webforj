package com.webforj.devtools.craftforj.inspector.contribution.appearance.selectdropdown;

import com.google.auto.service.AutoService;
import com.webforj.component.list.DwcSelectDropdown;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for DwcSelectDropdown max row count.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SelectDropdownMaxRowCountContribution
    extends ConcernContribution<DwcSelectDropdown<?>> {

  /**
   * Creates a new SelectDropdownMaxRowCountContribution.
   */
  public SelectDropdownMaxRowCountContribution() {
    super(DwcSelectDropdown.class, "MaxRowCount", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(DwcSelectDropdown::getMaxRowCount);
    setSetter((c, v) -> c.setMaxRowCount(v instanceof Number n ? n.intValue() : 0));
  }

}
