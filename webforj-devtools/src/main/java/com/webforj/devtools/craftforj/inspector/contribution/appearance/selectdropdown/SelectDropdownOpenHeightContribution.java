package com.webforj.devtools.craftforj.inspector.contribution.appearance.selectdropdown;

import com.google.auto.service.AutoService;
import com.webforj.component.list.DwcSelectDropdown;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for DwcSelectDropdown open height.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SelectDropdownOpenHeightContribution
    extends ConcernContribution<DwcSelectDropdown<?>> {

  /**
   * Creates a new SelectDropdownOpenHeightContribution.
   */
  public SelectDropdownOpenHeightContribution() {
    super(DwcSelectDropdown.class, "OpenHeight", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(DwcSelectDropdown::getOpenHeight);
    setSetter((c, v) -> c.setOpenHeight(v instanceof String s ? s : String.valueOf(v)));
  }

}
