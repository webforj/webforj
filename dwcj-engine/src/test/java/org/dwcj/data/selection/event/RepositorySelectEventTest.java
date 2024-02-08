package org.dwcj.data.selection.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.startup.type.BBjVector;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.dwcj.data.selection.MultipleSelectableComponentMock;
import org.junit.jupiter.api.Test;


class RepositorySelectEventTest {

  @Test
  void shouldGetSelectedIndex() {
    MultipleSelectableComponentMock component = new MultipleSelectableComponentMock();
    component.selectIndex(1);
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("index", 1);
    RepositorySelectEvent<MultipleSelectableComponentMock, String> event =
        new RepositorySelectEvent<>(component, eventMap);

    assertEquals(1, event.getSelectedIndex());
  }

  @Test
  void shouldGetSelectedItem() {
    MultipleSelectableComponentMock component = new MultipleSelectableComponentMock();
    component.selectIndex(1);
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("index", 1);
    RepositorySelectEvent<MultipleSelectableComponentMock, String> event =
        new RepositorySelectEvent<>(component, eventMap);

    assertEquals("item2", event.getSelectedItem());
  }

  @Test
  void shouldGetSelectedIndices() {
    MultipleSelectableComponentMock component = new MultipleSelectableComponentMock();
    component.selectIndex(1, 2);
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("indices", new BBjVector(List.of(1, 2)));
    RepositorySelectEvent<MultipleSelectableComponentMock, String> event =
        new RepositorySelectEvent<>(component, eventMap);

    assertEquals(List.of(1, 2), event.getSelectedIndices());
  }

  @Test
  void shouldGetSelectedItems() {
    MultipleSelectableComponentMock component = new MultipleSelectableComponentMock();
    component.selectIndex(1, 2);
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("indices", new BBjVector(List.of(1, 2)));
    RepositorySelectEvent<MultipleSelectableComponentMock, String> event =
        new RepositorySelectEvent<>(component, eventMap);

    assertEquals(List.of("item2", "item3"), event.getSelectedItems());
  }
}
