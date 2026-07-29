package com.webforj.devtools.craftforj.inspector.contribution.appearance.tree;

import com.google.auto.service.AutoService;
import com.webforj.component.tree.Tree;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Tree group icons visibility.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TreeGroupIconsVisibleContribution extends ConcernContribution<Tree> {

  /**
   * Creates a new TreeGroupIconsVisibleContribution.
   */
  public TreeGroupIconsVisibleContribution() {
    super(Tree.class, "GroupIconsVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Tree::isGroupIconsVisible);
    setSetter((c, v) -> c.setGroupIconsVisible(Boolean.TRUE.equals(v)));
  }

}
