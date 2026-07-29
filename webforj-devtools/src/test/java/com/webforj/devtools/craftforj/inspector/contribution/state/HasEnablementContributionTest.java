package com.webforj.devtools.craftforj.inspector.contribution.state;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasEnablement;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class HasEnablementContributionTest {

  private final HasEnablementContribution contribution = new HasEnablementContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    TestComponent component = mock(TestComponent.class);
    when(component.isEnabled()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Enabled", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    TestComponent component = mock(TestComponent.class);

    assertTrue(contribution.set(component, value));
    verify(component).setEnabled(value);
  }

  abstract static class TestComponent extends Component implements HasEnablement<Component> {
  }
}
