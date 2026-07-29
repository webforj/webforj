package com.webforj.devtools.craftforj.inspector.contribution.content.list;

import com.google.auto.service.AutoService;
import com.webforj.component.list.DwcList;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for list search field placeholder.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ListSearchPlaceholderContribution extends ConcernContribution<DwcList<?, ?>> {

  /**
   * Creates a new ListSearchPlaceholderContribution.
   */
  public ListSearchPlaceholderContribution() {
    super(DwcList.class, "SearchPlaceholder", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(c -> c.getSearch().getPlaceholder());
    setSetter((c, v) -> c.getSearch().setPlaceholder(String.valueOf(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setPlaceholder";
  }

  @Override
  public String getSourceAccessor() {
    return "getSearch";
  }
}
