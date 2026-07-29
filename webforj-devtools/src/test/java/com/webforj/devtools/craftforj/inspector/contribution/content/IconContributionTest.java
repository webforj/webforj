package com.webforj.devtools.craftforj.inspector.contribution.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconButton;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("IconContribution")
class IconContributionTest {

  private IconContribution contribution;
  private Icon icon;

  @BeforeEach
  void setUp() {
    contribution = new IconContribution();
    icon = mock(Icon.class);
  }

  @Test
  @DisplayName("supports Icon and IconButton components only")
  void shouldSupportIconComponentsOnly() {
    assertTrue(contribution.supports(icon));
    assertTrue(contribution.supports(mock(IconButton.class)));
    assertFalse(contribution.supports(mock(Component.class)));
  }

  @Test
  @DisplayName("uses the content category and Icon feature type")
  void shouldUseContentCategory() {
    assertEquals(FeatureCategory.CONTENT, contribution.getCategory());
    assertEquals("Icon", contribution.getFeatureType());
  }

  @Test
  @DisplayName("gets the icon as pool:name with the icon editor")
  void shouldGetPoolAndName() {
    when(icon.getPool()).thenReturn("tabler");
    when(icon.getName()).thenReturn("home");

    Optional<FeatureProperty> property = contribution.get(icon);

    assertTrue(property.isPresent());
    assertEquals("Icon", property.get().getName());
    assertEquals("tabler:home", property.get().getValue());
    assertEquals(PropertyType.ICON, property.get().getEditorType());
  }

  @Test
  @DisplayName("sets pool and name from a pool:name value")
  void shouldSetPoolAndName() {
    assertTrue(contribution.set(icon, "feather:bell"));

    verify(icon).setPool("feather");
    verify(icon).setName("bell");
  }

  @Test
  @DisplayName("rejects values that are not pool:name")
  void shouldRejectInvalidValue() {
    assertFalse(contribution.set(icon, "bell"));

    verify(icon, never()).setPool("bell");
    verify(icon, never()).setName("bell");
  }
}
