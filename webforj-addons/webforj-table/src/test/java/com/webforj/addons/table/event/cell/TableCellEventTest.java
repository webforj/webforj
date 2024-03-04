package com.webforj.addons.table.event.cell;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import com.webforj.addons.table.Table;
import com.webforj.component.window.Frame;
import com.webforj.data.EntityKeysRegistry;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TableCellEventTest {

  Table<String> table = null;

  @BeforeEach
  void setup() {
    table = new Table<String>();
    table.addColumn("item", String::valueOf).setLabel("Items");
    table.setItems(List.of("Item 0"));

    Frame frame = mock(Frame.class);
    frame.add(table);
  }

  @Test
  void shouldReturnClickedCell() {
    Map<String, Object> eventMap = new HashMap<>();
    EntityKeysRegistry registry = table.getItemKeysRegistry();
    String firstItem = table.getRepository().findByIndex(0).orElse(null);

    eventMap.put("itemKey", registry.getKey(firstItem));
    eventMap.put("columnKey", "item");

    TableCellEvent<String> event = new TableCellClickEvent<>(table, eventMap);
    assertEquals(table, event.getComponent());

    // row
    assertEquals(firstItem, event.getItem());
    assertEquals(registry.getKey(firstItem), event.getItemKey());

    // column
    assertEquals(table.getColumnById("item"), event.getColumn());
    assertEquals("item", event.getColumnKey());
  }
}
