package com.webforj.devtools.craftforj.inspector.contribution.appearance.selectdropdown;

import com.google.auto.service.AutoService;
import com.webforj.component.list.DwcSelectDropdown;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for DwcSelectDropdown open width.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SelectDropdownOpenWidthContribution extends ConcernContribution<DwcSelectDropdown<?>> {

  /**
   * Creates a new SelectDropdownOpenWidthContribution.
   */
  public SelectDropdownOpenWidthContribution() {
    super(DwcSelectDropdown.class, "OpenWidth", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(DwcSelectDropdown::getOpenWidth);
    setSetter((c, v) -> c.setOpenWidth(v instanceof String s ? s : String.valueOf(v)));
  }

}
