package com.webforj.devtools.craftforj.inspector.contribution.state.alert;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.alert.Alert;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AlertClosableContributionTest {

  private final AlertClosableContribution contribution = new AlertClosableContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Alert alert = mock(Alert.class);
    when(alert.isClosable()).thenReturn(value);

    var result = contribution.get(alert);

    assertTrue(result.isPresent());
    assertEquals("Closable", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Alert alert = mock(Alert.class);

    boolean success = contribution.set(alert, value);

    assertTrue(success);
    verify(alert).setClosable(value);
  }

}
