package com.webforj.devtools.craftforj.inspector.contribution.appearance.card;

import com.google.auto.service.AutoService;
import com.webforj.component.card.Card;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Card borderless mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class CardBorderlessContribution extends ConcernContribution<Card> {

  /**
   * Creates a new CardBorderlessContribution.
   */
  public CardBorderlessContribution() {
    super(Card.class, "Borderless", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Card::isBorderless);
    setSetter((c, v) -> c.setBorderless(Boolean.TRUE.equals(v)));
  }
}
