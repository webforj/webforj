package org.dwcj.component.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjListBox;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.dwcj.component.ReflectionUtils;
import org.dwcj.component.list.MultipleSelectableList.SelectionMode;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ListBoxTest {

  @Mock
  BBjListBox control;

  @InjectMocks
  ListBox component = new ListBox();

  @Nested
  @DisplayName("SelectionMode API")
  class SelectionModeApi {

    @ParameterizedTest
    @EnumSource(SelectionMode.class)
    @DisplayName("should set/get SelectionMode")
    void shouldSetSelectionModeAndGetIt(SelectionMode mode) throws BBjException {
      component.setSelectionMode(mode);

      assertEquals(mode, component.getSelectionMode());
      verify(control, times(1)).setMultipleSelection(mode == SelectionMode.MULTIPLE);
    }

    @Test
    @DisplayName("should throw DwcjRuntimeException when BBjException is thrown")
    void shouldThrowDwcjRuntimeExceptionWhenBBjExceptionIsThrown() throws BBjException {
      doThrow(BBjException.class).when(control).setMultipleSelection(true);

      assertThrows(DwcjRuntimeException.class,
          () -> component.setSelectionMode(SelectionMode.MULTIPLE));
    }
  }

  @Nested
  @DisplayName("MultipleSelection API")
  class MultipleSelectionApi {

    @Test
    @DisplayName("should deselect item by key")
    void shouldDeselectItemByKey() throws BBjException {
      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key", "value");
      component.selectIndex(0);

      component.deselectKey("key");

      verify(control, times(1)).deselectIndex(0);
    }

    @Test
    @DisplayName("should deselect item by index")
    void shouldDeselectItemByIndex() throws BBjException {
      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key", "value");
      component.selectIndex(0);

      component.deselectIndex(0);

      verify(control, times(1)).deselectIndex(0);
    }

    @Test
    @DisplayName("should throw BBjException as DwcjRuntimeException")
    void shouldThrowBBjExceptionAsDwcjRuntimeExceptionWhenBBjExceptionIsThrown()
        throws BBjException, IllegalAccessException {
      doThrow(BBjException.class).when(control).deselectIndex(0);
      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key", "value");
      component.selectIndex(0);

      assertThrows(DwcjRuntimeException.class, () -> component.deselectIndex(0));
    }

    @Test
    @DisplayName("should throw IllegalStateException when selection mode is single and deselect with no params")
    void shouldThrowIllegalStateExceptionWhenSelectionModeIsSingleAndDeselectWithNoParams() {
      component.setSelectionMode(SelectionMode.SINGLE);
      component.add("key", "value");

      assertThrows(IllegalStateException.class, () -> component.deselectIndex(0));
    }

    @Test
    @DisplayName("should throw IllegalArgumentException when item not found during deselect")
    void shouldThrowIllegalArgumentExceptionWhenItemNotFoundDuringDeselect() {
      component.setSelectionMode(SelectionMode.MULTIPLE);
      assertThrows(IllegalArgumentException.class,
          () -> component.deselect(new ListItem("key", "value")));
    }

    @Test
    @DisplayName("should deselect the first selected item when selection mode is single")
    void shouldDeselectTheFirstSelectedItemWhenSelectionModeIsSingle() throws BBjException {
      component.setSelectionMode(SelectionMode.SINGLE);
      component.add("key", "value");
      component.selectIndex(0);

      component.deselect();

      verify(control, times(1)).deselectIndex(0);

      reset(control);
      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.selectIndex(0);
      component.deselect();

      verify(control, times(1)).deselectIndex(0);
    }

    @Test
    @DisplayName("should deselect all selected items")
    void shouldDeselectAllSelectedItems() throws BBjException {
      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key1", "value1");
      component.add("key2", "value2");
      component.add("key3", "value3");
      component.selectKeys("key1", "key2", "key3");

      component.deselectAll();

      verify(control, times(1)).deselectAll();
    }

    @Test
    @DisplayName("should throw DwcjRuntimeException when deselectAll throws BBjException")
    void shouldThrowDwcjRuntimeExceptionWhenDeselectAllThrowsBBjException() throws Exception {
      component.setSelectionMode(SelectionMode.MULTIPLE);
      doThrow(BBjException.class).when(control).deselectAll();

      assertThrows(DwcjRuntimeException.class, () -> component.deselectAll());
    }

    @Test
    @DisplayName("should throw IllegalStateException when selection mode is single and selectAll is called")
    void shouldThrowIllegalStateExceptionWhenSelectionModeIsSingleAndSelectAllIsCalled() {
      component.setSelectionMode(SelectionMode.SINGLE);
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");

      assertThrows(IllegalStateException.class, () -> component.selectKeys("key-1", "key-2"));
    }

    @Test
    @DisplayName("should select items by keys")
    void shouldSelectItemsByKeys() throws BBjException {
      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      component.selectKeys("key-2", "key-3");

      verify(control, times(1)).setSelectedIndices(new BBjVector(Arrays.asList(1, 2)));

      doReturn(new BBjVector(Arrays.asList(1, 2))).when(control).getSelectedIndices();

      assertEquals(2, component.getSelectedItems().size());
    }

    @Test
    @DisplayName("should select items by indices")
    void shouldSelectItemsByIndices() throws BBjException {
      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      component.selectIndices(1, 2);

      verify(control, times(1)).setSelectedIndices(new BBjVector(Arrays.asList(1, 2)));

      doReturn(new BBjVector(Arrays.asList(1, 2))).when(control).getSelectedIndices();

      assertEquals(2, component.getSelectedItems().size());
    }

    @Test
    @DisplayName("should throw DwcjRuntimeException when select throws BBjException")
    void shouldThrowDwcjRuntimeExceptionWhenSelectThrowsBBjException() throws BBjException {
      doThrow(BBjException.class).when(control)
          .setSelectedIndices(new BBjVector(Arrays.asList(1, 2)));
      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      assertThrows(DwcjRuntimeException.class, () -> component.selectKeys("key-2", "key-3"));
    }

    @Test
    @DisplayName("should return selected indices when control is defined")
    void shouldReturnSelectedIndicesWhenControlIsDefined() throws BBjException {
      doReturn(new BBjVector(Arrays.asList(1, 2))).when(control).getSelectedIndices();

      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      assertEquals(2, component.getSelectedIndices().size());
      assertEquals(1, component.getSelectedIndices().get(0));
      assertEquals(2, component.getSelectedIndices().get(1));
    }

    @Test
    @DisplayName("should return selected indices when control is not defined")
    void shouldReturnSelectedIndicesWhenControlIsNotDefined() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      assertEquals(0, component.getSelectedIndices().size());

      component.selectIndices(1, 2);

      assertEquals(2, component.getSelectedIndices().size());
      assertEquals(1, component.getSelectedIndices().get(0));
      assertEquals(2, component.getSelectedIndices().get(1));
    }

    @Test
    @DisplayName("should return selected keys")
    void shouldReturnSelectedKeys() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      component.selectIndices(1, 2);

      assertEquals(2, component.getSelectedKeys().size());
      assertEquals("key-2", component.getSelectedKeys().get(0));
      assertEquals("key-3", component.getSelectedKeys().get(1));
    }

    @Test
    @DisplayName("should throw DwcjRuntimeException when getSelectedIndices throws BBjException")
    void shouldThrowDwcjRuntimeExceptionWhenGetSelectedIndicesThrowsBBjException()
        throws BBjException {
      doThrow(BBjException.class).when(control).getSelectedIndices();
      assertThrows(DwcjRuntimeException.class, () -> component.getSelectedIndices());
    }
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
    @DisplayName("should re-apply selection changes when selection mode is single")
    void shouldReApplySelectionChangesWhenSelectionModeIsSingle()
        throws BBjException, IllegalAccessException {
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
    @DisplayName("should re-apply selection changes when selection mode is multiple")
    void shouldReApplySelectionChangesWhenSelectionModeIsMultiple()
        throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setSelectionMode(SelectionMode.MULTIPLE);
      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");
      component.selectIndices(1, 2);

      assertEquals(2, component.getSelectedItems().size());
      assertEquals("key-2", component.getSelectedItems().get(0).getKey());
      assertEquals("key-3", component.getSelectedItems().get(1).getKey());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setSelectedIndices(new BBjVector(Arrays.asList(1, 2)));
    }

    @Test
    @DisplayName("should re-apply selection mode changes")
    void shouldReApplySelectionModeChanges() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setSelectionMode(SelectionMode.MULTIPLE);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setMultipleSelection(true);
    }
  }
}


