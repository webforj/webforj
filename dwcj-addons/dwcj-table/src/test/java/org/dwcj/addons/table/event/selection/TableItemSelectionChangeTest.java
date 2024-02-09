package org.dwcj.addons.table.event.selection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.dwcj.addons.table.Table;
import org.dwcj.component.window.Frame;
import org.dwcj.data.EntityKeysRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TableItemSelectionChangeTest {

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
  void shouldReturnSelectedRows() {
    Map<String, Object> eventMap = new HashMap<>();
    EntityKeysRegistry registry = table.getItemKeysRegistry();

    String firstItem = table.getRepository().findByIndex(0).orElse(null);
    eventMap.put("keys", Arrays.asList(registry.getKey(firstItem)));

    TableItemSelectionChange<String> event = new TableItemSelectionChange<String>(table, eventMap);

    assertEquals(table, event.getComponent());

    assertIterableEquals(List.of(firstItem), event.getSelectedItems());
    assertEquals(firstItem, event.getSelectedItem());

    assertIterableEquals(List.of(0), event.getSelectedIndices());
    assertEquals(0, event.getSelectedIndex());
  }
}
