package com.webforj.devtools.craftforj.inspector.contribution.appearance.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AppNavPinningTouchVisibleContributionTest {

  private final AppNavPinningTouchVisibleContribution contribution =
      new AppNavPinningTouchVisibleContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    AppNav component = mock(AppNav.class);
    AppNav.Pinning pinning = mock(AppNav.Pinning.class);
    when(component.getPinning()).thenReturn(pinning);
    when(pinning.isTouchVisible()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("PinningTouchVisible", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    AppNav component = mock(AppNav.class);
    AppNav.Pinning pinning = mock(AppNav.Pinning.class);
    when(component.getPinning()).thenReturn(pinning);

    assertTrue(contribution.set(component, value));
    verify(pinning).setTouchVisible(value);
  }

  @Test
  void shouldGenerateSourceThroughAccessor() {
    assertEquals("setTouchVisible", contribution.getSourceMethodName("PinningTouchVisible"));
    assertEquals("getPinning", contribution.getSourceAccessor());
  }
}
