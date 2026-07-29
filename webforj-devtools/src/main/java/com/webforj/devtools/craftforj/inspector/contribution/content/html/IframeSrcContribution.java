package com.webforj.devtools.craftforj.inspector.contribution.content.html;

import com.google.auto.service.AutoService;
import com.webforj.component.html.elements.Iframe;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Iframe source property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class IframeSrcContribution extends ConcernContribution<Iframe> {

  /**
   * Creates a new IframeSrcContribution.
   */
  public IframeSrcContribution() {
    super(Iframe.class, "Source", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(Iframe::getSrc);
    setSetter((c, v) -> c.setSrc(v != null ? v.toString() : ""));
  }

}
