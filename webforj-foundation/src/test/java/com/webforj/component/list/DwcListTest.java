package com.webforj.component.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.list.event.ListSelectEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcListTest {

  @Mock
  BBjListButton control;

  @InjectMocks
  DwcListMock component = new DwcListMock();

  @Nested
  @DisplayName("Add API")
  class AddApi {
    @Test
    @DisplayName("should add items")
    void shouldAddItems() throws BBjException {
      component.add("key", "value");

      assertEquals("value", component.getByKey("key").getText());
      verify(control, times(1)).addItem("value");
    }

    @Test
    @DisplayName("should throw DwcjRuntimeException when BBjException is thrown")
    void shouldThrowDwcjRuntimeExceptionOnBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).addItem(anyString());

      assertThrows(WebforjRuntimeException.class, () -> component.add("key", "value"));
    }

    @Test
    @DisplayName("should throw NullPointerException when item is null")
    void shouldThrowsNullPointerExceptionOnNullItem() {
      assertThrows(NullPointerException.class, () -> component.add((ListItem) null));
    }

    @Test
    @DisplayName("throws IllegalArgumentException when item is already in the list")
    void shouldThrowIllegalArgumentExceptionOnDuplicateItem() {
      ListItem item = new ListItem("key", "value");
      component.add(item);
      assertThrows(IllegalArgumentException.class, () -> component.add(item));
      assertThrows(IllegalArgumentException.class, () -> component.add("key", "value"));
    }
  }

  @Nested
  @DisplayName("Insert API")
  class InsertApi {
    @Test
    @DisplayName("should insert item at index")
    void shouldInsertItemAtSpecifiedIndex() throws BBjException {
      component.insert(0, "key-1", "value-1");
      component.insert(1, "key-2", "value-2");
      component.insert(1, "key-2-modified", "value-2-modified");

      assertEquals("value-1", component.getByKey("key-1").getText());
      assertEquals("value-2-modified", component.getByKey("key-2-modified").getText());

      verify(control, times(1)).insertItemAt(0, "value-1");
      verify(control, times(1)).insertItemAt(1, "value-2-modified");
    }

    @Test
    @DisplayName("should throw DwcjRuntimeException when BBjException is thrown")
    void shouldThrowDwcjRuntimeExceptionOnBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).insertItemAt(anyInt(), anyString());

      assertThrows(WebforjRuntimeException.class, () -> component.insert(0, "key", "value"));
    }

    @Test
    @DisplayName("should throw IndexOutOfBoundsException when index is out of bounds")
    void shouldThrowIndexOutOfBoundsExceptionOnInvalidIndex() {
      assertThrows(IndexOutOfBoundsException.class, () -> component.insert(-1, "key", "value"));
    }
  }

  @Nested
  @DisplayName("Insert Bulk API")
  class InsertBulkApi {
    @Test
    @DisplayName("should insert items at index")
    void shouldInsertItemsAtIndex() throws BBjException {
      List<ListItem> items =
          List.of(new ListItem("key-1", "value-1"), new ListItem("key-2", "value-2"));

      component.insert(0, items);

      assertTrue(component.containsKey("key-1"));
      assertTrue(component.containsKey("key-2"));

      verify(control, times(1)).insertItems(0,
          new BBjVector(items.stream().map(ListItem::getText).toList()));

      List<ListItem> newItems =
          List.of(new ListItem("key-3", "value-3"), new ListItem("key-4", "value-4"));

      component.insert(1, newItems);

      assertEquals("value-1", component.getByIndex(0).getText());
      assertEquals("value-3", component.getByIndex(1).getText());
      assertEquals("value-4", component.getByIndex(2).getText());
      assertEquals("value-2", component.getByIndex(3).getText());

      verify(control, times(1)).insertItems(1,
          new BBjVector(newItems.stream().map(ListItem::getText).toList()));
    }

    @Test
    @DisplayName("should throw DwcjRuntimeException when BBjException is thrown")
    @SuppressWarnings("squid:S5778")
    void shouldThrowDwcjRuntimeExceptionOnBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).insertItems(anyInt(), any(BBjVector.class));

      assertThrows(WebforjRuntimeException.class, () -> component.insert(0, new ArrayList<>()));
    }

    @Test
    @DisplayName("should throw IndexOutOfBoundsException when index is out of bounds")
    @SuppressWarnings("squid:S5778")
    void shouldThrowIndexOutOfBoundsExceptionOnInvalidIndex() {
      assertThrows(IndexOutOfBoundsException.class, () -> component.insert(-1, new ArrayList<>()));
    }

    @Test
    @DisplayName("should throw NullPointerException when items list is null")
    void shouldThrowNullPointerExceptionOnNullItemList() {
      assertThrows(NullPointerException.class, () -> component.insert(0, (List<ListItem>) null));
    }

    @Test
    @DisplayName("should throw NullPointerException when items list contains a null item")
    @SuppressWarnings("squid:S5778")
    void shouldThrowNullPointerExceptionOnNullItemInItemList() {
      assertThrows(NullPointerException.class,
          () -> component.insert(0, List.of(new ListItem("key", "value"), null)));
    }
  }

  @Nested
  @DisplayName("Get API")
  class GetApi {
    @Test
    @DisplayName("should get item by key")
    void shouldGetItemByKey() {
      component.add("key", "value");

      assertEquals("value", component.get("key").getText());
    }

    @Test
    @DisplayName("should get item by index")
    void shouldGetItemByIndex() {
      component.add("key", "value");

      assertEquals("value", component.getByIndex(0).getText());
    }

    @Test
    @DisplayName("should throw IndexOutOfBoundsException when index is out of bounds")
    void shouldThrowIndexOutOfBoundsExceptionOnInvalidIndex() {
      assertThrows(IndexOutOfBoundsException.class, () -> component.getByIndex(-1));
    }

    @Test
    @DisplayName("should iterate over the list")
    void shouldIterateOverTheList() {
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      for (ListItem entry : component) {
        assertEquals(entry.getKey(), entry.getKey());
      }
    }

    @Test
    @DisplayName("iterator should be Unmodifiable")
    @SuppressWarnings("squid:S5778")
    void iteratorShouldBeUnmodifiable() {
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      assertThrows(UnsupportedOperationException.class, () -> component.iterator().remove());
    }

    @Test
    @DisplayName("getItems should return unmodifiable list")
    @SuppressWarnings("squid:S5778")
    void getItemsShouldReturnUnmodifiableList() {
      component.add("key", "value");

      assertThrows(UnsupportedOperationException.class, () -> component.getItems().add(null));
    }
  }

  @Nested
  @DisplayName("Remove API")
  class RemoveApi {
    @Test
    @DisplayName("should remove item at index")
    void shouldRemoveItemAtIndex() throws BBjException {
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      component.remove(1);

      assertTrue(component.containsKey("key-1"));
      assertFalse(component.containsKey("key-2"));
      assertTrue(component.containsKey("key-3"));

      verify(control, times(1)).removeItemAt(1);
    }

    @Test
    @DisplayName("should remove item by key")
    void shouldRemoveItemByKey() {
      DwcListMock spy = spy(component);
      spy.add("key-1", "value-1");
      spy.add("key-2", "value-2");
      spy.add("key-3", "value-3");

      spy.remove("key-2");

      assertTrue(component.containsKey("key-1"));
      assertFalse(component.containsKey("key-2"));
      assertTrue(component.containsKey("key-3"));

      verify(spy, times(1)).remove(1);
    }

    @Test
    @DisplayName("should throw DwcjRuntimeException when BBjException is thrown")
    void shouldThrowDwcjRuntimeExceptionOnBBjException() throws BBjException {
      component.add("key", "value");
      doThrow(BBjException.class).when(control).removeItemAt(0);
      doThrow(BBjException.class).when(control).removeAllItems();

      assertThrows(WebforjRuntimeException.class, () -> component.remove("key"));
      assertThrows(WebforjRuntimeException.class, () -> component.removeAll());
    }

    @Test
    @DisplayName("should throw IndexOutOfBoundsException when index is out of bounds")
    void shouldThrowIndexOutOfBoundsExceptionOnInvalidIndex() {
      assertThrows(IndexOutOfBoundsException.class, () -> component.remove(-1));
    }

    @Test
    @DisplayName("should clears the list")
    void shouldClearsTheList() throws BBjException {
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      component.removeAll();

      assertTrue(component.isEmpty());
      verify(control, times(1)).removeAllItems();
    }
  }

  @Test
  @DisplayName("should monitor items' value changes and update the client")
  void shouldMonitorItemsValueChangesAndUpdateTheClient() throws BBjException {
    component.add("value-1");
    component.insert(1, "value-2");

    component.getByIndex(0).setText("value-1-modified");
    component.getByIndex(1).setText("value-2-modified");

    verify(control, times(1)).setTextAt(0, "value-1-modified");
    verify(control, times(1)).setTextAt(1, "value-2-modified");

    component.insert(2,
        List.of(new ListItem("key-3", "value-3"), new ListItem("key-4", "value-4")));

    component.getByIndex(2).setText("value-3-modified");
    component.getByIndex(3).setText("value-4-modified");

    verify(control, times(1)).setTextAt(2, "value-3-modified");
    verify(control, times(1)).setTextAt(3, "value-4-modified");
  }

  @Nested
  @DisplayName("Selection API")
  class SelectionApi {

    @Test
    @DisplayName("should select item by index")
    void shouldSelectItemByIndex() throws BBjException {
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      component.selectIndex(1);

      verify(control, times(1)).selectIndex(1);
    }

    @Test
    @DisplayName("should select item by key")
    void shouldSelectItemByKey() throws BBjException {
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      component.selectKey("key-2");

      verify(control, times(1)).selectIndex(1);
    }

    @Test
    @DisplayName("should throw IllegalArgumentException when the given item is not in the list or given index is out of bounds")
    void shouldThrowIllegalArgumentExceptionOnInvalidItemOrIndex() {
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      assertThrows(IllegalArgumentException.class, () -> component.selectKey("key-4"));
      assertThrows(IllegalArgumentException.class,
          () -> component.select(new ListItem("Does not exist")));
      assertThrows(IndexOutOfBoundsException.class, () -> component.selectIndex(3));
    }
  }

  @Test
  @DisplayName("should get selected item when control is not defined")
  void shouldGetSelectedItemWhenControlIsNotDefined() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.add("key-1", "value-1");
    component.add("key-2", "value-2");
    component.add("key-3", "value-3");

    component.selectIndex(1);

    assertEquals("value-2", component.getSelected().getText());
  }

  @Test
  @DisplayName("should get selected item when control is defined")
  void shouldGetSelectedItemWhenControlIsDefined() throws BBjException {
    doReturn(1).when(control).getSelectedIndex();
    component.add("key-1", "value-1");
    component.add("key-2", "value-2");
    component.add("key-3", "value-3");

    component.selectIndex(1);

    assertEquals("value-2", component.getSelected().getText());
  }

  @Test
  @DisplayName("should throw DwcjRuntimeException when BBjException is thrown")
  void shouldThrowDwcjRuntimeExceptionOnBBjException() throws BBjException {
    component.add("key-1", "value-1");
    doThrow(BBjException.class).when(control).selectIndex(anyInt());
    doThrow(BBjException.class).when(control).getSelectedIndex();

    assertThrows(WebforjRuntimeException.class, () -> component.selectIndex(0));
    assertThrows(WebforjRuntimeException.class, () -> component.getSelected());
  }

  @Test
  @DisplayName("adding/removing supported events")
  void addingRemovingSupportedEvents() {
    EventListener<ListSelectEvent<ListItem>> selectListener = event -> {
    };

    ListenerRegistration<ListSelectEvent<ListItem>> r = component.onSelect(selectListener);
    assertEquals(1, component.getEventListeners(ListSelectEvent.class).size());

    r.remove();
    assertEquals(0, component.getEventListeners(ListSelectEvent.class).size());
  }

  @Test
  void shouldSetGetHelperText() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setHelperText("helper text");
    assertEquals("helper text", component.getHelperText());

    assertEquals("helper text", component.getProperty("helperText"));
  }
}
