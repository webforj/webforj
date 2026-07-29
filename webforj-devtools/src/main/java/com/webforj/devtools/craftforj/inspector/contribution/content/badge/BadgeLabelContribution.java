package com.webforj.devtools.craftforj.inspector.contribution.content.badge;

import com.google.auto.service.AutoService;
import com.webforj.component.badge.Badge;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Badge label.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class BadgeLabelContribution extends ConcernContribution<Badge> {

  /**
   * Creates a new BadgeLabelContribution.
   */
  public BadgeLabelContribution() {
    super(Badge.class, "Label", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(Badge::getLabel);
    setSetter((c, v) -> c.setLabel(String.valueOf(v)));
  }
}
