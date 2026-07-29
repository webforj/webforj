package com.webforj.devtools.craftforj.inspector.contribution.state.checkbox;

import com.google.auto.service.AutoService;
import com.webforj.component.optioninput.CheckBox;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the CheckBox checked property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class CheckBoxCheckedContribution extends ConcernContribution<CheckBox> {

  /**
   * Creates the CheckBox checked contribution.
   */
  public CheckBoxCheckedContribution() {
    super(CheckBox.class, "Checked", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(CheckBox::isChecked);
    setSetter((c, v) -> c.setChecked(Boolean.TRUE.equals(v)));
  }

}
