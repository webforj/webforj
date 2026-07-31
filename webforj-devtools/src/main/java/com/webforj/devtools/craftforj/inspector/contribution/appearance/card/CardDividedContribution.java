package com.webforj.devtools.craftforj.inspector.contribution.appearance.card;

import com.google.auto.service.AutoService;
import com.webforj.component.card.Card;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Card divided mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class CardDividedContribution extends ConcernContribution<Card> {

  /**
   * Creates a new CardDividedContribution.
   */
  public CardDividedContribution() {
    super(Card.class, "Divided", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Card::isDivided);
    setSetter((c, v) -> c.setDivided(Boolean.TRUE.equals(v)));
  }
}
