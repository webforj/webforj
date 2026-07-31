package com.webforj.devtools.craftforj.inspector.contribution.appearance.card;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.card.Card;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class CardShadowContributionTest {

  private final CardShadowContribution contribution = new CardShadowContribution();

  @Test
  void shouldGet() {
    Card card = mock(Card.class);
    when(card.getShadow()).thenReturn(Card.Shadow.LARGE);

    var result = contribution.get(card);

    assertTrue(result.isPresent());
    assertEquals("Shadow", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.card.Card.Shadow.LARGE", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Card card = mock(Card.class);

    boolean success = contribution.set(card, Card.Shadow.NONE);

    assertTrue(success);
    verify(card).setShadow(Card.Shadow.NONE);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Card card = mock(Card.class);
    assertEquals(Card.Shadow.class, contribution.findEnumClass(card));
  }
}
