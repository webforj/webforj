package com.webforj.devtools.craftforj.inspector.contribution.appearance.toolbar;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.toolbar.Toolbar;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Toolbar compact property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ToolbarCompactContribution extends ConcernContribution<Toolbar> {

  /**
   * Creates a new ToolbarCompactContribution.
   */
  public ToolbarCompactContribution() {
    super(Toolbar.class, "Compact", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Toolbar::isCompact);
    setSetter((c, v) -> c.setCompact(Boolean.TRUE.equals(v)));
  }

}
