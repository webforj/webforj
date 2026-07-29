package com.webforj.devtools.craftforj.inspector.contribution.state.alert;

import com.google.auto.service.AutoService;
import com.webforj.component.alert.Alert;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Alert closable property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AlertClosableContribution extends ConcernContribution<Alert> {

  /**
   * Creates a new AlertClosableContribution.
   */
  public AlertClosableContribution() {
    super(Alert.class, "Closable", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Alert::isClosable);
    setSetter((c, v) -> c.setClosable(Boolean.TRUE.equals(v)));
  }

}
