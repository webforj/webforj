package com.webforj.devtools.craftforj.inspector.contribution.appearance.card;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.card.Card;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class CardBorderlessContributionTest {

  private final CardBorderlessContribution contribution = new CardBorderlessContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Card component = mock(Card.class);
    when(component.isBorderless()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Borderless", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Card component = mock(Card.class);

    assertTrue(contribution.set(component, value));
    verify(component).setBorderless(value);
  }
}
