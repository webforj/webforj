package com.webforj.devtools.craftforj.inspector.contribution.state.radiobutton;

import com.google.auto.service.AutoService;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for RadioButton checked state.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class RadioButtonCheckedContribution extends ConcernContribution<RadioButton> {

  /**
   * Creates a new RadioButtonCheckedContribution.
   */
  public RadioButtonCheckedContribution() {
    super(RadioButton.class, "Checked", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(RadioButton::isChecked);
    setSetter((c, v) -> c.setChecked(Boolean.TRUE.equals(v)));
  }

}
