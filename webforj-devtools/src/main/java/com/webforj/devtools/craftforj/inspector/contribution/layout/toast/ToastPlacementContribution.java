package com.webforj.devtools.craftforj.inspector.contribution.layout.toast;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.toast.Toast;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Toast placement property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ToastPlacementContribution extends EnumConcernContribution<Toast> {

  /**
   * Creates a new ToastPlacementContribution.
   */
  public ToastPlacementContribution() {
    super(Toast.class, "Placement", FeatureCategory.LAYOUT);
    setGetter(Toast::getPlacement);
    setSetter((c, v) -> c.setPlacement((Toast.Placement) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Toast.Placement.class;
  }

}
