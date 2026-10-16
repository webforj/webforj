package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasClassName;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.ListConcernContribution;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the HasClassName concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasClassNameContribution extends ListConcernContribution<HasClassName<?>> {

  /**
   * Creates the HasClassName contribution.
   */
  public HasClassNameContribution() {
    super(HasClassName.class, "ClassNames", FeatureCategory.APPEARANCE);
    setGetter(HasClassName::getClassNameList);
    setAddHandler(HasClassName::addClassName);
    setRemoveHandler(HasClassName::removeClassName);
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "addClassName";
  }
}
