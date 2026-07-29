package com.webforj.devtools.craftforj.inspector.contribution.content;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasTooltip;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasTooltip concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasTooltipContribution extends ConcernContribution<HasTooltip<?>> {

  /**
   * Creates the HasTooltip contribution.
   */
  public HasTooltipContribution() {
    super(HasTooltip.class, "TooltipText", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(HasTooltip::getTooltipText);
    setSetter((c, v) -> c.setTooltipText(String.valueOf(v)));
  }
}
