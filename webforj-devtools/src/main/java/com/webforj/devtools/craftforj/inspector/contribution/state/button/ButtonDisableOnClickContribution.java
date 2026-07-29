package com.webforj.devtools.craftforj.inspector.contribution.state.button;

import com.google.auto.service.AutoService;
import com.webforj.component.button.DwcButton;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the Button disableOnClick property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ButtonDisableOnClickContribution extends ConcernContribution<DwcButton<?>> {

  /**
   * Creates the Button disableOnClick contribution.
   */
  public ButtonDisableOnClickContribution() {
    super(DwcButton.class, "DisableOnClick", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(DwcButton::isDisableOnClick);
    setSetter((c, v) -> c.setDisableOnClick(Boolean.TRUE.equals(v)));
  }

}
