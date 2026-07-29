package com.webforj.devtools.craftforj.inspector.contribution.layout.splitter;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.splitter.Splitter;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Splitter position relative property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SplitterPositionRelativeContribution extends ConcernContribution<Splitter> {

  /**
   * Creates a new SplitterPositionRelativeContribution.
   */
  public SplitterPositionRelativeContribution() {
    super(Splitter.class, "PositionRelative", FeatureCategory.LAYOUT);
    setBuilderConfig(b -> b.decimal(1.0, 0, 100));
    setGetter(Splitter::getPositionRelative);
    setSetter((c, v) -> c.setPositionRelative(((Number) v).doubleValue()));
  }

}
