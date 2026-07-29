package com.webforj.devtools.craftforj.inspector.contribution.content.list;

import com.google.auto.service.AutoService;
import com.webforj.component.list.DwcList;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for list search empty message.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ListSearchEmptyMessageContribution extends ConcernContribution<DwcList<?, ?>> {

  /**
   * Creates a new ListSearchEmptyMessageContribution.
   */
  public ListSearchEmptyMessageContribution() {
    super(DwcList.class, "SearchEmptyMessage", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(c -> c.getSearch().getEmptyMessage());
    setSetter((c, v) -> c.getSearch().setEmptyMessage(String.valueOf(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setEmptyMessage";
  }

  @Override
  public String getSourceAccessor() {
    return "getSearch";
  }
}
