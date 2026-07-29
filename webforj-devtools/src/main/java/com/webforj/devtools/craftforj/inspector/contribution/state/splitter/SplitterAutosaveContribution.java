package com.webforj.devtools.craftforj.inspector.contribution.state.splitter;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.splitter.Splitter;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Splitter autosave property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class SplitterAutosaveContribution extends ConcernContribution<Splitter> {

  /**
   * Creates a new SplitterAutosaveContribution.
   */
  public SplitterAutosaveContribution() {
    super(Splitter.class, "Autosave", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Splitter::isAutosave);
    setSetter((c, v) -> c.setAutosave(Boolean.TRUE.equals(v)));
  }

}
