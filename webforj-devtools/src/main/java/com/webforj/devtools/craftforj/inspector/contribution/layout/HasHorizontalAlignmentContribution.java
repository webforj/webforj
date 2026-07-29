package com.webforj.devtools.craftforj.inspector.contribution.layout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.concern.HasHorizontalAlignment.Alignment;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the HasHorizontalAlignment concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasHorizontalAlignmentContribution
    extends EnumConcernContribution<HasHorizontalAlignment<?>> {

  /**
   * Creates the HasHorizontalAlignment contribution.
   */
  @SuppressWarnings("rawtypes")
  public HasHorizontalAlignmentContribution() {
    super(HasHorizontalAlignment.class, "HorizontalAlignment", FeatureCategory.LAYOUT);
    setGetter(HasHorizontalAlignment::getHorizontalAlignment);
    setSetter((c, v) -> ((HasHorizontalAlignment) c).setHorizontalAlignment((Alignment) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Alignment.class;
  }
}
