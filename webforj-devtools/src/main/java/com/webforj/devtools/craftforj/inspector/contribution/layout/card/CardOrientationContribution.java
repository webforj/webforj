package com.webforj.devtools.craftforj.inspector.contribution.layout.card;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.card.Card;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Card orientation property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class CardOrientationContribution extends EnumConcernContribution<Card> {

  /**
   * Creates a new CardOrientationContribution.
   */
  public CardOrientationContribution() {
    super(Card.class, "Orientation", FeatureCategory.LAYOUT);
    setGetter(Card::getOrientation);
    setSetter((c, v) -> c.setOrientation((Card.Orientation) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Card.Orientation.class;
  }
}
