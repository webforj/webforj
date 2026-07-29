package com.webforj.devtools.craftforj.inspector.contribution.state.tree;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.tree.Tree;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Tree selection mode.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TreeSelectionModeContribution extends EnumConcernContribution<Tree> {

  /**
   * Creates a new TreeSelectionModeContribution.
   */
  public TreeSelectionModeContribution() {
    super(Tree.class, "SelectionMode", FeatureCategory.STATE);
    setGetter(Tree::getSelectionMode);
    setSetter((c, v) -> c.setSelectionMode((Tree.SelectionMode) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Tree.SelectionMode.class;
  }

}
