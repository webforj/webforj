package com.webforj.devtools.craftforj.inspector.contribution.content.avatar;

import com.google.auto.service.AutoService;
import com.webforj.component.avatar.Avatar;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Avatar initials property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AvatarInitialsContribution extends ConcernContribution<Avatar> {

  /**
   * Creates a new AvatarInitialsContribution.
   */
  public AvatarInitialsContribution() {
    super(Avatar.class, "Initials", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(Avatar::getInitials);
    setSetter((c, v) -> c.setInitials(v != null ? v.toString() : ""));
  }

}
