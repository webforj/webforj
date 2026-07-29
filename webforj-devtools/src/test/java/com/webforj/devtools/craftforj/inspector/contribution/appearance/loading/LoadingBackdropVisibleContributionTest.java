package com.webforj.devtools.craftforj.inspector.contribution.appearance.loading;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.loading.Loading;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class LoadingBackdropVisibleContributionTest {

  private final LoadingBackdropVisibleContribution contribution =
      new LoadingBackdropVisibleContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Loading loading = mock(Loading.class);
    when(loading.isBackdropVisible()).thenReturn(value);

    var result = contribution.get(loading);

    assertTrue(result.isPresent());
    assertEquals("BackdropVisible", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Loading loading = mock(Loading.class);

    boolean success = contribution.set(loading, value);

    assertTrue(success);
    verify(loading).setBackdropVisible(value);
  }

}
