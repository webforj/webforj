package com.webforj.devtools.craftforj.inspector.contribution.state.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TableSelectionModeContributionTest {

  private final TableSelectionModeContribution contribution = new TableSelectionModeContribution();

  @Test
  void shouldGet() {
    Table<?> component = mock(Table.class);
    when(component.getSelectionMode()).thenReturn(Table.SelectionMode.SINGLE);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SelectionMode", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(Table.SelectionMode.class.getCanonicalName() + ".SINGLE", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Table<?> component = mock(Table.class);

    assertTrue(
        contribution.set(component, Table.SelectionMode.class.getCanonicalName() + ".MULTIPLE"));
    verify(component).setSelectionMode(Table.SelectionMode.MULTIPLE);
  }
}
