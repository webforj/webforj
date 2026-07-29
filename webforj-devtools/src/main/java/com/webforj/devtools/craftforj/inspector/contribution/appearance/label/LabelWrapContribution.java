package com.webforj.devtools.craftforj.inspector.contribution.appearance.label;

import com.google.auto.service.AutoService;
import com.webforj.component.text.Label;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Label wrap property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class LabelWrapContribution extends ConcernContribution<Label> {

  /**
   * Creates a new contribution.
   */
  public LabelWrapContribution() {
    super(Label.class, "Wrap", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Label::isWrap);
    setSetter((c, v) -> c.setWrap(Boolean.TRUE.equals(v)));
  }

}
