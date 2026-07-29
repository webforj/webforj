package com.webforj.devtools.craftforj.inspector.contribution.content.html;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.html.elements.Img;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("ImgSrcContribution")
class ImgSrcContributionTest {

  private ImgSrcContribution contribution;
  private Img img;

  @BeforeEach
  void setUp() {
    contribution = new ImgSrcContribution();
    img = mock(Img.class);
  }

  @Test
  @DisplayName("supports Img components only")
  void shouldSupportImgComponentsOnly() {
    assertTrue(contribution.supports(img));
    assertFalse(contribution.supports(mock(Component.class)));
  }

  @Test
  @DisplayName("uses the content category and ImgSrc feature type")
  void shouldUseContentCategory() {
    assertEquals(FeatureCategory.CONTENT, contribution.getCategory());
    assertEquals("ImgSrc", contribution.getFeatureType());
  }

  @Test
  @DisplayName("gets the source with the Source property name")
  void shouldGetSource() {
    when(img.getSrc()).thenReturn("https://example.com/logo.png");

    Optional<FeatureProperty> property = contribution.get(img);

    assertTrue(property.isPresent());
    assertEquals("Source", property.get().getName());
    assertEquals("https://example.com/logo.png", property.get().getValue());
  }

  @Test
  @DisplayName("sets the source")
  void shouldSetSource() {
    assertTrue(contribution.set(img, "https://example.com/logo.png"));

    verify(img).setSrc("https://example.com/logo.png");
  }
}
