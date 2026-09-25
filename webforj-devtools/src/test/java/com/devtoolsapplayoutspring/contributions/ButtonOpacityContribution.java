package com.devtoolsapplayoutspring.contributions;

import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.contribution.KeyValueConcernContribution;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/** A real application extension exposing one keyed style property. */
public class ButtonOpacityContribution extends KeyValueConcernContribution<Button> {

  /** Creates the opacity property handler. */
  public ButtonOpacityContribution() {
    super(Button.class, "opacity", "Opacity", FeatureCategory.APPEARANCE);
    setGetter(button -> button.getStyle("opacity"));
    setSetter((button, value) -> {
      if (value == null || String.valueOf(value).isEmpty()) {
        button.removeStyle("opacity");
      } else {
        button.setStyle("opacity", String.valueOf(value));
      }
    });
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setStyle";
  }
}
