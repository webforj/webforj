package com.webforj.devtools.craftforj.inspector.contribution.state.checkbox;

import com.google.auto.service.AutoService;
import com.webforj.component.optioninput.CheckBox;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the CheckBox indeterminate property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class CheckBoxIndeterminateContribution extends ConcernContribution<CheckBox> {

  /**
   * Creates the CheckBox indeterminate contribution.
   */
  public CheckBoxIndeterminateContribution() {
    super(CheckBox.class, "Indeterminate", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(CheckBox::isIndeterminate);
    setSetter((c, v) -> c.setIndeterminate(Boolean.TRUE.equals(v)));
  }

}
