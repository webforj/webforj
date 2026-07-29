package com.webforj.devtools.craftforj.inspector.contribution.content.html;

import com.google.auto.service.AutoService;
import com.webforj.component.html.elements.Img;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Img alt property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ImgAltContribution extends ConcernContribution<Img> {

  /**
   * Creates a new ImgAltContribution.
   */
  public ImgAltContribution() {
    super(Img.class, "Alt", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(Img::getAlt);
    setSetter((c, v) -> c.setAlt(v != null ? v.toString() : ""));
  }

}
