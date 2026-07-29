package com.webforj.devtools.craftforj.inspector.contribution.layout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasTextPosition;
import com.webforj.concern.HasTextPosition.Position;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the HasTextPosition concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasTextPositionContribution extends EnumConcernContribution<HasTextPosition<?>> {

  /**
   * Creates the HasTextPosition contribution.
   */
  public HasTextPositionContribution() {
    super(HasTextPosition.class, "TextPosition", FeatureCategory.LAYOUT);
    setGetter(HasTextPosition::getTextPosition);
    setSetter((c, v) -> c.setTextPosition((Position) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Position.class;
  }
}
