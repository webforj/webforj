package com.webforj.devtools.craftforj.inspector.contribution.content.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AppNavPinningTitleContributionTest {

  private final AppNavPinningTitleContribution contribution = new AppNavPinningTitleContribution();

  @Test
  void shouldGet() {
    AppNav component = mock(AppNav.class);
    AppNav.Pinning pinning = mock(AppNav.Pinning.class);
    when(component.getPinning()).thenReturn(pinning);
    when(pinning.getTitle()).thenReturn("Pinned");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("PinningTitle", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Pinned", result.get().getValue());
  }

  @Test
  void shouldSet() {
    AppNav component = mock(AppNav.class);
    AppNav.Pinning pinning = mock(AppNav.Pinning.class);
    when(component.getPinning()).thenReturn(pinning);

    assertTrue(contribution.set(component, "Favorites"));
    verify(pinning).setTitle("Favorites");
  }

  @Test
  void shouldGenerateSourceThroughAccessor() {
    assertEquals("setTitle", contribution.getSourceMethodName("PinningTitle"));
    assertEquals("getPinning", contribution.getSourceAccessor());
  }
}
