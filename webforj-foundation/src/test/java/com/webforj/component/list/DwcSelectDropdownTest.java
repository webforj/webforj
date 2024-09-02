package com.webforj.component.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.component.Component;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.list.event.ListClickEvent;
import com.webforj.component.list.event.ListCloseEvent;
import com.webforj.component.list.event.ListOpenEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.List;
import java.util.stream.Collectors;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcSelectDropdownTest {

  @Mock
  BBjListButton control;

  @InjectMocks
  DwcSelectDropdownMock component = new DwcSelectDropdownMock();

  @Test
  @DisplayName("open should open the list")
  void shouldOpenList() throws IllegalAccessException, BBjException {
    component.open();
    verify(control, times(1)).openList();
  }

  @Test
  @DisplayName("close should close the list")
  void shouldCloseList() throws IllegalAccessException, BBjException {
    component.close();
    verify(control, times(1)).closeList();
  }

  @Test
  @DisplayName("open/close should throw DwcjRuntimeException if BBjException is thrown")
  void shouldThrowDwcjRuntimeException() throws IllegalAccessException, BBjException {
    doThrow(BBjException.class).when(control).openList();
    assertThrows(WebforjRuntimeException.class, () -> component.open());

    doThrow(BBjException.class).when(control).closeList();
    assertThrows(WebforjRuntimeException.class, () -> component.close());
  }

  @Test
  @DisplayName("isOpen should return true if the list is open, false otherwise")
  void shouldReturnTrueIfListIsOpen() throws IllegalAccessException, BBjException {
    doReturn("true").when(control).getProperty("opened", Object.class);
    assertEquals(true, component.isOpened());

    doReturn("false").when(control).getProperty("opened", Object.class);
    assertEquals(false, component.isOpened());

    ReflectionUtils.nullifyControl(component);

    component.open();
    assertEquals(true, component.isOpened());

    component.close();
    assertEquals(false, component.isOpened());
  }

  @Test
  @DisplayName("setDropdownMaxWidth should set the maximum width of the dropdown")
  void shouldSetDropdownMaxWidth() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setOpenWidth(100);

    assertEquals("100px", component.getOpenWidth());
    assertEquals("100px", component.getProperty("openWidth"));
  }

  @Test
  @DisplayName("setDropdownMaxHeight should set the dropdown max height")
  void shouldSetDropdownMaxHeight() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setOpenHeight(100);

    assertEquals("100px", component.getOpenHeight());
    assertEquals("100px", component.getProperty("openHeight"));
  }

  @Test
  @DisplayName("deselect remove selection when control is null")
  void shouldRemoveSelectionWhenControlIsNull() throws IllegalAccessException, BBjException {
    ReflectionUtils.nullifyControl(component);
    component.insert("item 1", "item 2");
    component.selectIndex(1);
    assertEquals(1, component.getSelectedIndex());

    component.deselect();
    assertEquals(-1, component.getSelectedIndex());
    verify(control, times(0)).deselect();
  }

  @Test
  @DisplayName("deselect remove selection when control is defined")
  void shouldRemoveSelectionWhenControlIsDefined() throws IllegalAccessException, BBjException {
    component.insert("item 1", "item 2");
    component.selectIndex(1);

    component.deselect();
    verify(control, times(1)).deselect();
  }

  @Test
  @DisplayName("adding supported events")
  void addingSupportedEvents() {
    EventListener<ListOpenEvent> openListener = event -> {
    };
    EventListener<ListCloseEvent> closeListener = event -> {
    };
    EventListener<ListClickEvent> clickListener = event -> {
    };

    component.onOpen(openListener);
    component.onClose(closeListener);
    component.onClick(clickListener);

    assertEquals(1, component.getEventListeners(ListOpenEvent.class).size());
    assertEquals(1, component.getEventListeners(ListCloseEvent.class).size());
    assertEquals(1, component.getEventListeners(ListClickEvent.class).size());
  }

  @Nested
  @DisplayName("OnAttach")
  class OnAttach {
    @Test
    @DisplayName("should re-apply items changes")
    void shouldReApplyItemsChanges() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      List<ListItem> items =
          List.of(new ListItem("key-1", "value-1"), new ListItem("key-2", "value-2"));
      component.insert(0, items);
      assertEquals(2, component.size());
      verify(control, times(0)).insertItems(anyInt(), any(BBjVector.class));

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      assertEquals(2, component.size());
      verify(control, times(1)).insertItems(0,
          new BBjVector(items.stream().map(ListItem::getText).collect(Collectors.toList())));
    }

    @Test
    @DisplayName("should re-apply selection changes")
    void shouldReApplySelectionChanges() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.selectIndex(1);

      assertEquals(component.getByIndex(1), component.getSelected());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).selectIndex(1);
    }

    @Test
    @DisplayName("should re-apply open changes")
    void shouldReApplyOpenChanges() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.open();

      assertEquals(true, component.isOpened());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).openList();
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    @DisplayName("adding/removing supported events")
    void addingRemovingSupportedEvents() {
      EventListener<ListOpenEvent> openListener = event -> {
      };
      EventListener<ListCloseEvent> closeListener = event -> {
      };
      EventListener<ListClickEvent> clickListener = event -> {
      };

      ListenerRegistration<ListOpenEvent> r1 = component.onOpen(openListener);
      ListenerRegistration<ListCloseEvent> r2 = component.onClose(closeListener);
      ListenerRegistration<ListClickEvent> r3 = component.onClick(clickListener);

      assertEquals(1, component.getEventListeners(ListOpenEvent.class).size());
      assertEquals(1, component.getEventListeners(ListCloseEvent.class).size());
      assertEquals(1, component.getEventListeners(ListClickEvent.class).size());

      r1.remove();
      r2.remove();
      r3.remove();

      assertEquals(0, component.getEventListeners(ListOpenEvent.class).size());
      assertEquals(0, component.getEventListeners(ListCloseEvent.class).size());
      assertEquals(0, component.getEventListeners(ListClickEvent.class).size());
    }
  }

  @Test
  void shouldConfigureMaxRowCount() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    DwcSelectDropdownMock spy = spy(component);
    spy.setMaxRowCount(5);
    assertEquals(5, spy.getMaxRowCount());

    assertEquals(5.0, component.getProperty("maxRowCount"));
  }

  @Test
  void shouldConfigureDropdownType() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setDropdownType("custom-type");
    assertEquals("custom-type", component.getDropdownType());

    assertEquals("custom-type", component.getProperty("type"));
  }

  @Nested
  class SlotsApi {

    @Test
    void shouldSetAndGetPrefix() {
      Component prefixComponent = mock(Component.class);
      component.setPrefixComponent(prefixComponent);
      assertEquals(prefixComponent, component.getPrefixComponent());
    }

    @Test
    void shouldSetAndGetSuffix() {
      Component suffixComponent = mock(Component.class);
      component.setSuffixComponent(suffixComponent);
      assertEquals(suffixComponent, component.getSuffixComponent());
    }
  }
}
