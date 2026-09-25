package com.webforj.devtools.craftforj.inspector.contribution.content.accordion;

import com.google.auto.service.AutoService;
import com.webforj.component.accordion.AccordionPanel;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.List;
import java.util.Map;

/**
 * Contribution for AccordionPanel header label.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AccordionPanelLabelContribution extends ConcernContribution<AccordionPanel> {

  /**
   * Creates a new AccordionPanelLabelContribution.
   */
  public AccordionPanelLabelContribution() {
    super(AccordionPanel.class, "Label", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(AccordionPanel::getLabel);
    setSetter((c, v) -> c.setLabel(String.valueOf(v)));
  }

  /** {@inheritDoc} */
  @Override
  public Map<String, List<String>> getSourceMethodExpansions() {
    return Map.of("setText", List.of("setLabel"));
  }
}
