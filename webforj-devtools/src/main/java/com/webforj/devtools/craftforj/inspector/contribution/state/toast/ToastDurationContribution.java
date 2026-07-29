package com.webforj.devtools.craftforj.inspector.contribution.state.toast;

import com.google.auto.service.AutoService;
import com.webforj.component.toast.Toast;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Toast duration property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ToastDurationContribution extends ConcernContribution<Toast> {

  /**
   * Creates a new ToastDurationContribution.
   */
  public ToastDurationContribution() {
    super(Toast.class, "Duration", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(Toast::getDuration);
    setSetter((c, v) -> c.setDuration(((Number) v).intValue()));
  }

}
