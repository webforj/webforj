package com.webforj.devtools.craftforj.inspector.contribution.content;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasText;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.TextSourceAliases;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.List;
import java.util.Map;

/**
 * Contribution for the HasText concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasTextContribution extends ConcernContribution<HasText<?>> {

  /**
   * Creates the HasText contribution.
   */
  public HasTextContribution() {
    super(HasText.class, "Text", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(HasText::getText);
    setSetter((c, v) -> c.setText(String.valueOf(v)));
  }

  /** {@inheritDoc} */
  @Override
  public Map<String, List<String>> getSourceMethodExpansions(Class<?> componentType) {
    return TextSourceAliases.getAliases(componentType, "setText");
  }
}
