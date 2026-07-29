package com.webforj.devtools.craftforj.inspector.contribution.content.button;

import com.google.auto.service.AutoService;
import com.webforj.component.button.DwcButton;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Button name property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ButtonNameContribution extends ConcernContribution<DwcButton<?>> {

  /**
   * Creates the Button name contribution.
   */
  public ButtonNameContribution() {
    super(DwcButton.class, "Name", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(DwcButton::getName);
    setSetter((c, v) -> c.setName(v != null ? v.toString() : ""));
  }

}
