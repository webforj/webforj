package com.webforj.devtools.craftforj.inspector.contribution.state.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TableMultiSelectWithClickContributionTest {

  private final TableMultiSelectWithClickContribution contribution =
      new TableMultiSelectWithClickContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Table<?> component = mock(Table.class);
    when(component.isMultiSelectWithClick()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MultiSelectWithClick", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Table<?> component = mock(Table.class);

    assertTrue(contribution.set(component, value));
    verify(component).setMultiSelectWithClick(value);
  }
}
