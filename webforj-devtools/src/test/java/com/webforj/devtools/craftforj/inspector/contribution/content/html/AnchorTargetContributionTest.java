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

@DisplayName("AnchorTargetContribution")
class AnchorTargetContributionTest {

  private AnchorTargetContribution contribution;
  private Anchor anchor;

  @BeforeEach
  void setUp() {
    contribution = new AnchorTargetContribution();
    anchor = mock(Anchor.class);
  }

  @Test
  @DisplayName("supports Anchor components only")
  void shouldSupportAnchorComponentsOnly() {
    assertTrue(contribution.supports(anchor));
    assertFalse(contribution.supports(mock(Component.class)));
  }

  @Test
  @DisplayName("uses the content category and AnchorTarget feature type")
  void shouldUseContentCategory() {
    assertEquals(FeatureCategory.CONTENT, contribution.getCategory());
    assertEquals("AnchorTarget", contribution.getFeatureType());
  }

  @Test
  @DisplayName("gets the target with the Target property name")
  void shouldGetTarget() {
    when(anchor.getTarget()).thenReturn("_blank");

    Optional<FeatureProperty> property = contribution.get(anchor);

    assertTrue(property.isPresent());
    assertEquals("Target", property.get().getName());
    assertEquals("_blank", property.get().getValue());
  }

  @Test
  @DisplayName("sets the target")
  void shouldSetTarget() {
    assertTrue(contribution.set(anchor, "_blank"));

    verify(anchor).setTarget("_blank");
  }
}
