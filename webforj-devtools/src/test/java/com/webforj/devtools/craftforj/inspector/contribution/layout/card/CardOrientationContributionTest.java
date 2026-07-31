package com.webforj.devtools.craftforj.inspector.contribution.layout.card;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.card.Card;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class CardOrientationContributionTest {

  private final CardOrientationContribution contribution = new CardOrientationContribution();

  @Test
  void shouldGet() {
    Card card = mock(Card.class);
    when(card.getOrientation()).thenReturn(Card.Orientation.VERTICAL);

    var result = contribution.get(card);

    assertTrue(result.isPresent());
    assertEquals("Orientation", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.card.Card.Orientation.VERTICAL", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Card card = mock(Card.class);

    boolean success = contribution.set(card, Card.Orientation.HORIZONTAL);

    assertTrue(success);
    verify(card).setOrientation(Card.Orientation.HORIZONTAL);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Card card = mock(Card.class);
    assertEquals(Card.Orientation.class, contribution.findEnumClass(card));
  }
}
