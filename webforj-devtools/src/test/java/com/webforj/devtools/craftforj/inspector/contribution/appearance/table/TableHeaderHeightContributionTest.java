package com.webforj.devtools.craftforj.inspector.contribution.appearance.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.table.Table;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TableHeaderHeightContributionTest {

  private final TableHeaderHeightContribution contribution = new TableHeaderHeightContribution();

  @Test
  void shouldGet() {
    Table<?> component = mock(Table.class);
    when(component.getHeaderHeight()).thenReturn(50.0);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("HeaderHeight", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(50.0, result.get().getValue());
  }

  @Test
  void shouldSet() {
    Table<?> component = mock(Table.class);

    assertTrue(contribution.set(component, 60.0));
    verify(component).setHeaderHeight(60.0);
  }
}
