package org.dwcj.addons.table.event.item;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.dwcj.addons.table.Table;
import org.dwcj.component.window.Frame;
import org.dwcj.data.EntityKeysRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TableItemEventTest {

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
  void shouldReturnClickedRow() {
    Map<String, Object> eventMap = new HashMap<>();
    EntityKeysRegistry registry = table.getItemKeysRegistry();
    String firstItem = table.getRepository().findByIndex(0).orElse(null);

    eventMap.put("key", registry.getKey(firstItem));
    eventMap.put("index", 0);

    TableItemClickEvent<String> event = new TableItemClickEvent<>(table, eventMap);

    assertEquals(table, event.getComponent());

    assertEquals(firstItem, event.getItem());
    assertEquals(0, event.getItemIndex());
    assertEquals(registry.getKey(firstItem), event.getItemKey());
  }
}
