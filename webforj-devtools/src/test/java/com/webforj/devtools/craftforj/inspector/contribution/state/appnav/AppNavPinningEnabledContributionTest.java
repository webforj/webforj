package com.webforj.devtools.craftforj.inspector.contribution.state.appnav;

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

class AppNavPinningEnabledContributionTest {

  private final AppNavPinningEnabledContribution contribution =
      new AppNavPinningEnabledContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    AppNav component = mock(AppNav.class);
    AppNav.Pinning pinning = mock(AppNav.Pinning.class);
    when(component.getPinning()).thenReturn(pinning);
    when(pinning.isEnabled()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("PinningEnabled", result.get().getName());
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
    verify(pinning).setEnabled(value);
  }

  @Test
  void shouldGenerateSourceThroughAccessor() {
    assertEquals("setEnabled", contribution.getSourceMethodName("PinningEnabled"));
    assertEquals("getPinning", contribution.getSourceAccessor());
  }
}
