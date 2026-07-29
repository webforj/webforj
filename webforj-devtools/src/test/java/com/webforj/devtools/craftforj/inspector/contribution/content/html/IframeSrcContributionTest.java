package com.webforj.devtools.craftforj.inspector.contribution.content.html;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.html.elements.Iframe;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("IframeSrcContribution")
class IframeSrcContributionTest {

  private IframeSrcContribution contribution;
  private Iframe iframe;

  @BeforeEach
  void setUp() {
    contribution = new IframeSrcContribution();
    iframe = mock(Iframe.class);
  }

  @Test
  @DisplayName("supports Iframe components only")
  void shouldSupportIframeComponentsOnly() {
    assertTrue(contribution.supports(iframe));
    assertFalse(contribution.supports(mock(Component.class)));
  }

  @Test
  @DisplayName("uses the content category and IframeSrc feature type")
  void shouldUseContentCategory() {
    assertEquals(FeatureCategory.CONTENT, contribution.getCategory());
    assertEquals("IframeSrc", contribution.getFeatureType());
  }

  @Test
  @DisplayName("gets the source with the Source property name")
  void shouldGetSource() {
    when(iframe.getSrc()).thenReturn("https://example.com/embed");

    Optional<FeatureProperty> property = contribution.get(iframe);

    assertTrue(property.isPresent());
    assertEquals("Source", property.get().getName());
    assertEquals("https://example.com/embed", property.get().getValue());
  }

  @Test
  @DisplayName("sets the source")
  void shouldSetSource() {
    assertTrue(contribution.set(iframe, "https://example.com/embed"));

    verify(iframe).setSrc("https://example.com/embed");
  }
}
