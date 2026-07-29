package com.webforj.devtools.craftforj.inspector.contribution.appearance.avatar;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.avatar.Avatar;
import com.webforj.component.avatar.AvatarShape;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Avatar shape property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AvatarShapeContribution extends EnumConcernContribution<Avatar> {

  /**
   * Creates a new AvatarShapeContribution.
   */
  public AvatarShapeContribution() {
    super(Avatar.class, "Shape", FeatureCategory.APPEARANCE);
    setGetter(Avatar::getShape);
    setSetter((c, v) -> c.setShape((AvatarShape) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return AvatarShape.class;
  }

}
