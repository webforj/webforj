package com.webforj.devtools.craftforj.inspector.contribution.state.navigator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.navigator.Navigator;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class NavigatorAutoDisableContributionTest {

  private final NavigatorAutoDisableContribution contribution =
      new NavigatorAutoDisableContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Navigator component = mock(Navigator.class);
    when(component.isAutoDisable()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("AutoDisable", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Navigator component = mock(Navigator.class);

    assertTrue(contribution.set(component, value));
    verify(component).setAutoDisable(value);
  }
}
