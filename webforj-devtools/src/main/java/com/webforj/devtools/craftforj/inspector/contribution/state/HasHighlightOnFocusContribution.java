package com.webforj.devtools.craftforj.inspector.contribution.state;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.concern.HasHighlightOnFocus.Behavior;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the HasHighlightOnFocus concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasHighlightOnFocusContribution
    extends EnumConcernContribution<HasHighlightOnFocus<?>> {

  /**
   * Creates the HasHighlightOnFocus contribution.
   */
  public HasHighlightOnFocusContribution() {
    super(HasHighlightOnFocus.class, "HighlightOnFocus", FeatureCategory.STATE);
    setGetter(HasHighlightOnFocus::getHighlightOnFocus);
    setSetter((c, v) -> (c).setHighlightOnFocus((Behavior) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Behavior.class;
  }
}
