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

@DisplayName("ImgAltContribution")
class ImgAltContributionTest {

  private ImgAltContribution contribution;
  private Img img;

  @BeforeEach
  void setUp() {
    contribution = new ImgAltContribution();
    img = mock(Img.class);
  }

  @Test
  @DisplayName("supports Img components only")
  void shouldSupportImgComponentsOnly() {
    assertTrue(contribution.supports(img));
    assertFalse(contribution.supports(mock(Component.class)));
  }

  @Test
  @DisplayName("uses the content category and ImgAlt feature type")
  void shouldUseContentCategory() {
    assertEquals(FeatureCategory.CONTENT, contribution.getCategory());
    assertEquals("ImgAlt", contribution.getFeatureType());
  }

  @Test
  @DisplayName("gets the alt text with the Alt property name")
  void shouldGetAlt() {
    when(img.getAlt()).thenReturn("Company logo");

    Optional<FeatureProperty> property = contribution.get(img);

    assertTrue(property.isPresent());
    assertEquals("Alt", property.get().getName());
    assertEquals("Company logo", property.get().getValue());
  }

  @Test
  @DisplayName("sets the alt text")
  void shouldSetAlt() {
    assertTrue(contribution.set(img, "Company logo"));

    verify(img).setAlt("Company logo");
  }
}
