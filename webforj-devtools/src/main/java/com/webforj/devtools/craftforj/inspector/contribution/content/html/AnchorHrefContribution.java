package com.webforj.devtools.craftforj.inspector.contribution.content.html;

import com.google.auto.service.AutoService;
import com.webforj.component.html.elements.Anchor;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Anchor href property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AnchorHrefContribution extends ConcernContribution<Anchor> {

  /**
   * Creates a new AnchorHrefContribution.
   */
  public AnchorHrefContribution() {
    super(Anchor.class, "Href", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(Anchor::getHref);
    setSetter((c, v) -> c.setHref(v != null ? v.toString() : ""));
  }

}
