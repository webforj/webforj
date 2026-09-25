package com.webforj.devtools.craftforj.inspector.contribution.content;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasLabel;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.TextSourceAliases;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.List;
import java.util.Map;

/**
 * Contribution for the HasLabel concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasLabelContribution extends ConcernContribution<HasLabel<?>> {

  /**
   * Creates the HasLabel contribution.
   */
  public HasLabelContribution() {
    super(HasLabel.class, "Label", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(HasLabel::getLabel);
    setSetter((c, v) -> c.setLabel(String.valueOf(v)));
  }

  /** {@inheritDoc} */
  @Override
  public Map<String, List<String>> getSourceMethodExpansions(Class<?> componentType) {
    return TextSourceAliases.getAliases(componentType, "setLabel");
  }
}
