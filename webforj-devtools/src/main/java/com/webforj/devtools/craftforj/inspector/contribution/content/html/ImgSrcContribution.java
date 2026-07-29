package com.webforj.devtools.craftforj.inspector.contribution.content.html;

import com.google.auto.service.AutoService;
import com.webforj.component.html.elements.Img;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Img source property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ImgSrcContribution extends ConcernContribution<Img> {

  /**
   * Creates a new ImgSrcContribution.
   */
  public ImgSrcContribution() {
    super(Img.class, "Source", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(Img::getSrc);
    setSetter((c, v) -> c.setSrc(v != null ? v.toString() : ""));
  }

}
