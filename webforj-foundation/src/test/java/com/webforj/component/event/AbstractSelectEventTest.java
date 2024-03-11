package com.webforj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.startup.type.BBjVector;
import com.webforj.component.event.mocks.MultipleSelectableComponentMock;
import com.webforj.component.event.mocks.SelectEventMock;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;


class AbstractSelectEventTest {

  @Test
  void shouldGetSelectedIndex() {
    MultipleSelectableComponentMock component = new MultipleSelectableComponentMock();
    component.selectIndex(1);
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("index", 1);
    SelectEventMock<MultipleSelectableComponentMock, String> event =
        new SelectEventMock<>(component, eventMap);

    assertEquals(1, event.getSelectedIndex());
  }

  @Test
  void shouldGetSelectedItem() {
    MultipleSelectableComponentMock component = new MultipleSelectableComponentMock();
    component.selectIndex(1);
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("index", 1);
    SelectEventMock<MultipleSelectableComponentMock, String> event =
        new SelectEventMock<>(component, eventMap);

    assertEquals("item2", event.getSelectedItem());
  }

  @Test
  void shouldGetSelectedIndices() {
    MultipleSelectableComponentMock component = new MultipleSelectableComponentMock();
    component.selectIndex(1, 2);
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("indices", new BBjVector(List.of(1, 2)));
    SelectEventMock<MultipleSelectableComponentMock, String> event =
        new SelectEventMock<>(component, eventMap);

    assertEquals(List.of(1, 2), event.getSelectedIndices());
  }

  @Test
  void shouldGetSelectedItems() {
    MultipleSelectableComponentMock component = new MultipleSelectableComponentMock();
    component.selectIndex(1, 2);
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("indices", new BBjVector(List.of(1, 2)));
    SelectEventMock<MultipleSelectableComponentMock, String> event =
        new SelectEventMock<>(component, eventMap);

    assertEquals(List.of("item2", "item3"), event.getSelectedItems());
  }
}
