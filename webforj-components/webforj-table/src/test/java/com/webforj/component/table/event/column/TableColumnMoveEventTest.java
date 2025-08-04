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

class TableColumnMoveEventTest {

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
    eventMap.put("oldIndex", "2");
    eventMap.put("newIndex", "5");

    TableColumnMoveEvent event = new TableColumnMoveEvent(table, eventMap);

    assertNotNull(event);
    assertEquals(table, event.getComponent());
    assertEquals(column, event.getColumn());
    assertEquals(2, event.getOldIndex());
    assertEquals(5, event.getNewIndex());
  }

  @Test
  void shouldHandleIntegerOrderValues() {
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("id", "testColumn");
    eventMap.put("oldIndex", 0);
    eventMap.put("newIndex", 3);

    TableColumnMoveEvent event = new TableColumnMoveEvent(table, eventMap);

    assertEquals(0, event.getOldIndex());
    assertEquals(3, event.getNewIndex());
  }

  @Test
  void shouldThrowExceptionForNonExistentColumn() {
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("id", "nonExistentColumn");
    eventMap.put("oldIndex", "1");
    eventMap.put("newIndex", "2");

    TableColumnMoveEvent event = new TableColumnMoveEvent(table, eventMap);

    assertThrows(IllegalArgumentException.class, () -> event.getColumn(),
        "Column with ID 'nonExistentColumn' not found");
  }
}
