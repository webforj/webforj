package com.webforj.component.table.event.column;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.component.table.Column;
import com.webforj.component.table.Table;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TableColumnResizeEventTest {

  private Table<Object> table;
  private Column<Object, String> column;

  @BeforeEach
  void setUp() {
    table = new Table<>();
    column = table.addColumn("testColumn", obj -> obj.toString().toUpperCase());
  }

  @Test
  void shouldCreateEventWithCorrectValues() {
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("id", "testColumn");
    eventMap.put("oldWidth", "100");
    eventMap.put("newWidth", "150");

    TableColumnResizeEvent event = new TableColumnResizeEvent(table, eventMap);

    assertNotNull(event);
    assertEquals(table, event.getComponent());
    assertEquals(column, event.getColumn());
    assertEquals(100, event.getOldWidth());
    assertEquals(150, event.getNewWidth());
  }

  @Test
  void shouldHandleIntegerWidthValues() {
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("id", "testColumn");
    eventMap.put("oldWidth", 100);
    eventMap.put("newWidth", 200);

    TableColumnResizeEvent event = new TableColumnResizeEvent(table, eventMap);

    assertEquals(100.0, event.getOldWidth());
    assertEquals(200.0, event.getNewWidth());
  }

  @Test
  void shouldThrowExceptionForNonExistentColumn() {
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("id", "nonExistentColumn");
    eventMap.put("oldWidth", "100");
    eventMap.put("newWidth", "150");

    TableColumnResizeEvent event = new TableColumnResizeEvent(table, eventMap);

    assertThrows(IllegalArgumentException.class, () -> event.getColumn(),
        "Column with ID 'nonExistentColumn' not found");
  }
}
