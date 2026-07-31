package com.webforj.devtools.craftforj.inspector.contribution.appearance.card;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.card.Card;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Card shadow property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class CardShadowContribution extends EnumConcernContribution<Card> {

  /**
   * Creates a new CardShadowContribution.
   */
  public CardShadowContribution() {
    super(Card.class, "Shadow", FeatureCategory.APPEARANCE);
    setGetter(Card::getShadow);
    setSetter((c, v) -> c.setShadow((Card.Shadow) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Card.Shadow.class;
  }
}
