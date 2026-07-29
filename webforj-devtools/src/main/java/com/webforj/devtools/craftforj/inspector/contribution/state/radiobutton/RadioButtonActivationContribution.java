package com.webforj.devtools.craftforj.inspector.contribution.state.radiobutton;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for RadioButton activation type.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class RadioButtonActivationContribution extends EnumConcernContribution<RadioButton> {

  /**
   * Creates a new RadioButtonActivationContribution.
   */
  public RadioButtonActivationContribution() {
    super(RadioButton.class, "Activation", FeatureCategory.STATE);
    setGetter(RadioButton::getActivation);
    setSetter((c, v) -> c.setActivation((RadioButton.Activation) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return RadioButton.Activation.class;
  }

}
