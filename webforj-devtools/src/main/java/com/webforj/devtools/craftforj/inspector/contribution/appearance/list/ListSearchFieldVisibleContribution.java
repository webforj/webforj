package com.webforj.devtools.craftforj.inspector.contribution.appearance.list;

import com.google.auto.service.AutoService;
import com.webforj.component.list.DwcList;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for list search field visibility.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ListSearchFieldVisibleContribution extends ConcernContribution<DwcList<?, ?>> {

  /**
   * Creates a new ListSearchFieldVisibleContribution.
   */
  public ListSearchFieldVisibleContribution() {
    super(DwcList.class, "SearchFieldVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(c -> c.getSearch().isFieldVisible());
    setSetter((c, v) -> c.getSearch().setFieldVisible(Boolean.TRUE.equals(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setFieldVisible";
  }

  @Override
  public String getSourceAccessor() {
    return "getSearch";
  }
}
