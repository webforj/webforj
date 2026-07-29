package com.webforj.devtools.craftforj.inspector.contribution.state.combobox;

import com.google.auto.service.AutoService;
import com.webforj.component.list.ComboBox;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for ComboBox allow custom value.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ComboBoxAllowCustomValueContribution extends ConcernContribution<ComboBox> {

  /**
   * Creates a new ComboBoxAllowCustomValueContribution.
   */
  public ComboBoxAllowCustomValueContribution() {
    super(ComboBox.class, "AllowCustomValue", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(ComboBox::isAllowCustomValue);
    setSetter((c, v) -> c.setAllowCustomValue(Boolean.TRUE.equals(v)));
  }

}
