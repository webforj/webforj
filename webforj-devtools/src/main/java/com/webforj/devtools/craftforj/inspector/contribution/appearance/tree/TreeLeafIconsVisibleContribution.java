package com.webforj.devtools.craftforj.inspector.contribution.appearance.tree;

import com.google.auto.service.AutoService;
import com.webforj.component.tree.Tree;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Tree leaf icons visibility.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TreeLeafIconsVisibleContribution extends ConcernContribution<Tree> {

  /**
   * Creates a new TreeLeafIconsVisibleContribution.
   */
  public TreeLeafIconsVisibleContribution() {
    super(Tree.class, "LeafIconsVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Tree::isLeafIconsVisible);
    setSetter((c, v) -> c.setLeafIconsVisible(Boolean.TRUE.equals(v)));
  }

}
