package com.webforj.devtools.craftforj.inspector.contribution.appearance.tree;

import com.google.auto.service.AutoService;
import com.webforj.component.tree.Tree;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Tree connecting lines visibility.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TreeConnectedContribution extends ConcernContribution<Tree> {

  /**
   * Creates a new TreeConnectedContribution.
   */
  public TreeConnectedContribution() {
    super(Tree.class, "Connected", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Tree::isConnected);
    setSetter((c, v) -> c.setConnected(Boolean.TRUE.equals(v)));
  }

}
