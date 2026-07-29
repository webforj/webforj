package com.webforj.devtools.craftforj.inspector.contribution.appearance.radiobutton;

import com.google.auto.service.AutoService;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for RadioButton switch rendering.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class RadioButtonSwitchContribution extends ConcernContribution<RadioButton> {

  /**
   * Creates a new RadioButtonSwitchContribution.
   */
  public RadioButtonSwitchContribution() {
    super(RadioButton.class, "Switch", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(RadioButton::isSwitch);
    setSetter((c, v) -> c.setSwitch(Boolean.TRUE.equals(v)));
  }

}
