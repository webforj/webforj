package com.webforj.devtools.craftforj.inspector.contribution.layout.splitter;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.splitter.Splitter;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Splitter orientation property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SplitterOrientationContribution extends EnumConcernContribution<Splitter> {

  /**
   * Creates a new SplitterOrientationContribution.
   */
  public SplitterOrientationContribution() {
    super(Splitter.class, "Orientation", FeatureCategory.LAYOUT);
    setGetter(Splitter::getOrientation);
    setSetter((c, v) -> c.setOrientation((Splitter.Orientation) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Splitter.Orientation.class;
  }

}
