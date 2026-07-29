package com.webforj.devtools.craftforj.inspector.contribution.content.html;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.html.elements.Anchor;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("AnchorHrefContribution")
class AnchorHrefContributionTest {

  private AnchorHrefContribution contribution;
  private Anchor anchor;

  @BeforeEach
  void setUp() {
    contribution = new AnchorHrefContribution();
    anchor = mock(Anchor.class);
  }

  @Test
  @DisplayName("supports Anchor components only")
  void shouldSupportAnchorComponentsOnly() {
    assertTrue(contribution.supports(anchor));
    assertFalse(contribution.supports(mock(Component.class)));
  }

  @Test
  @DisplayName("uses the content category and AnchorHref feature type")
  void shouldUseContentCategory() {
    assertEquals(FeatureCategory.CONTENT, contribution.getCategory());
    assertEquals("AnchorHref", contribution.getFeatureType());
  }

  @Test
  @DisplayName("gets the href with the Href property name")
  void shouldGetHref() {
    when(anchor.getHref()).thenReturn("https://webforj.com");

    Optional<FeatureProperty> property = contribution.get(anchor);

    assertTrue(property.isPresent());
    assertEquals("Href", property.get().getName());
    assertEquals("https://webforj.com", property.get().getValue());
  }

  @Test
  @DisplayName("sets the href")
  void shouldSetHref() {
    assertTrue(contribution.set(anchor, "https://webforj.com"));

    verify(anchor).setHref("https://webforj.com");
  }
}
