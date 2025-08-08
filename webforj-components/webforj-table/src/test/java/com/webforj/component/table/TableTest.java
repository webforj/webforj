package com.webforj.component.table;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.component.element.Element;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.table.event.TableSortChangeEvent;
import com.webforj.component.table.event.cell.TableCellClickEvent;
import com.webforj.component.table.event.cell.TableCellDoubleClickEvent;
import com.webforj.component.table.event.column.TableColumnMoveEvent;
import com.webforj.component.table.event.column.TableColumnResizeEvent;
import com.webforj.component.table.event.item.TableItemClickEvent;
import com.webforj.component.table.event.item.TableItemDoubleClickEvent;
import com.webforj.component.table.event.selection.TableItemDeselectEvent;
import com.webforj.component.table.event.selection.TableItemSelectEvent;
import com.webforj.component.table.event.selection.TableItemSelectionChange;
import com.webforj.component.table.renderer.Renderer;
import com.webforj.data.HasEntityKey;
import com.webforj.data.repository.Repository;
import com.webforj.dispatcher.EventListener;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TableTest {

  private Table<String> component;

  @BeforeEach
  void setUp() {
    component = new Table<>();
  }

  @Nested
  class Properties {
    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Table.class, component, descriptor -> {
          return !Arrays.asList("columnDefinitions", "data", "getRowId", "selected")
              .contains(descriptor.getName());
        });
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldSetGetBorders() {
      var borders = EnumSet.of(Table.Border.AROUND, Table.Border.ROWS, Table.Border.COLUMNS);
      component.setBordersVisible(borders);

      assertEquals(borders, component.getBordersVisible());
      assertTrue(component.getProperty("border", Boolean.class));
      assertTrue(component.getProperty("columnsBorder", Boolean.class));
      assertTrue(component.getProperty("rowsBorder", Boolean.class));

      component.setBordersVisible(EnumSet.noneOf(Table.Border.class));

      assertFalse(component.getProperty("border", Boolean.class));
      assertFalse(component.getProperty("columnsBorder", Boolean.class));
      assertFalse(component.getProperty("rowsBorder", Boolean.class));
    }
  }

  @Nested
  class ColumnsApi {
    @Test
    void shouldAddColumnWithSpecificId() {
      String columnId = "testColumn";
      Function<String, String> provider = Function.identity();
      Column<String, String> column = component.addColumn(columnId, provider);

      assertNotNull(column);
      assertEquals(columnId, column.getId());
      assertEquals(provider, column.getValueProvider());
    }

    @Test
    void shouldAddColumnWithOnlyValueProvider() {
      Function<String, String> provider = Function.identity();
      Column<String, String> column = component.addColumn(provider);

      assertNotNull(column);
      assertNotNull(column.getId());
      assertEquals(provider, column.getValueProvider());
    }

    @Test
    void shouldAddColumnWithRenderer() {
      Renderer<String> renderer = spy(Renderer.class);
      Column<String, String> column = component.addColumn(renderer);

      assertNotNull(column);
      assertNotNull(column.getId());
      assertNotNull(column.getValueProvider());
      assertEquals("", column.getLabel());
      assertEquals(renderer, column.getRenderer());
    }

    @Test
    void shouldGetColumnById() {
      String columnId = "testColumn";
      component.addColumn(columnId, Function.identity());
      Column<String, ?> foundColumn = component.getColumnById(columnId);

      assertNotNull(foundColumn);
      assertEquals(columnId, foundColumn.getId());
    }

    @Test
    void getColumnByIdShouldReturnNullIfColumnDoesNotExist() {
      String nonexistentId = "nonexistentId";
      Column<String, ?> foundColumn = component.getColumnById(nonexistentId);

      assertNull(foundColumn);
    }

    @Test
    void shouldCheckIfColumnExists() {
      String columnId = "testColumn";
      component.addColumn(columnId, Function.identity());

      assertTrue(component.hasColumn(columnId));
      assertFalse(component.hasColumn("nonexistentId"));
    }

    @Test
    void shouldGetUnmodifiableColumns() {
      String columnId = "testColumn";
      component.addColumn(columnId, Function.identity());

      List<Column<String, ?>> columns = component.getColumns();

      assertThrows(UnsupportedOperationException.class, () -> columns.add(null));
    }

    @Test
    void shouldRemoveColumn() {
      String columnId = "testColumn";
      Column<String, ?> c = component.addColumn(columnId, Function.identity());
      component.removeColumn(c);

      assertFalse(component.hasColumn(columnId));
    }

    @Test
    void shouldThrowExceptionWhenMovingNonExistentColumn() {
      Table<String> table = new Table<>();

      assertThrows(IllegalArgumentException.class, () -> table.moveColumn("nonexistent", 0),
          "Column with id 'nonexistent' not found");
    }

    @Test
    void shouldThrowExceptionWhenMovingHiddenColumn() {
      Table<String> table = new Table<>();
      Column<String, String> column = table.addColumn("hidden", Function.identity());
      column.setHidden(true);

      assertThrows(IllegalArgumentException.class, () -> table.moveColumn("hidden", 0),
          "Cannot move hidden column with id 'hidden'");
    }

    @Test
    void shouldThrowExceptionWhenMovingToInvalidIndex() {
      Table<String> table = new Table<>();
      table.addColumn("col1", Function.identity());
      table.addColumn("col2", Function.identity());

      assertThrows(IndexOutOfBoundsException.class, () -> table.moveColumn("col1", -1),
          "Column index out of visible columns bounds: -1");

      assertThrows(IndexOutOfBoundsException.class, () -> table.moveColumn("col1", 2),
          "Column index out of visible columns bounds: 2");
    }

    @Test
    void shouldMoveColumn() {
      Table<String> table = spy(new Table<>());
      Element elMock = mock(Element.class);
      when(table.el()).thenReturn(elMock);
      when(elMock.callJsFunctionAsync("moveColumn", "col1", 1))
          .thenReturn(PendingResult.completedWith(null));

      table.addColumn("col1", Function.identity());
      table.addColumn("col2", Function.identity());

      PendingResult<Void> result = table.moveColumn("col1", 1);

      verify(elMock).callJsFunctionAsync("moveColumn", "col1", 1);
      assertNotNull(result);
    }
  }

  @Nested
  class RowsApi {
    @Test
    void shouldSetItems() {
      List<String> items = Arrays.asList("item1", "item2");
      component.setItems(items);

      assertIterableEquals(items, component.getItems());
    }
  }

  @Nested
  class SelectionApi {
    @Test
    void shouldSelectGivenRows() {
      Table<Person> table = spy(new Table<>());
      Person person1 = new Person("John");
      Person person2 = new Person("Jane");

      table.setItems(Arrays.asList(person1, person2));
      table.select(person1);

      assertTrue(table.isSelected(person1));
      assertFalse(table.isSelected(person2));
    }

    @Test
    void shouldDeselectGivenRows() {
      Table<Person> table = spy(new Table<>());
      Person person1 = new Person("John");
      Person person2 = new Person("Jane");

      table.setItems(Arrays.asList(person1, person2));
      table.select(person1);
      table.deselect(person1);

      assertFalse(table.isSelected(person2));
    }

    @Test
    void shouldReturnSelectedRows() {
      Table<Person> table = new Table<>();
      Person person1 = new Person("Fuck");
      Person person2 = new Person("Jane");

      table.setItems(Arrays.asList(person1, person2));
      table.select(person1);

      assertIterableEquals(new HashSet<>(Arrays.asList(person1)), table.getSelectedItems());
    }

    @Test
    void shouldReturnFirstSelectedRow() {
      Table<Person> table = spy(new Table<>());
      Person person1 = new Person("John");
      Person person2 = new Person("Jane");

      table.setItems(Arrays.asList(person1, person2));
      table.select(person2, person1);

      assertNotNull(table.getSelected());
    }

    @Test
    void shouldSelectAllRows() {
      Table<Person> table = spy(new Table<>());
      Person person1 = new Person("John");
      Person person2 = new Person("Jane");

      table.setItems(Arrays.asList(person1, person2));
      table.selectAll();

      assertTrue(table.isSelected(person1));
      assertTrue(table.isSelected(person2));
    }

    @Test
    void shouldDeselectAllRows() {
      Table<Person> table = spy(new Table<>());
      Person person1 = new Person("John");
      Person person2 = new Person("Jane");

      table.setItems(Arrays.asList(person1, person2));
      table.selectAll();
      table.deselectAll();

      assertFalse(table.isSelected(person1));
      assertFalse(table.isSelected(person2));
    }

    @Test
    void shouldSetValue() {
      Table<Person> table = spy(new Table<>());
      Person person1 = new Person("John");
      Person person2 = new Person("Jane");

      table.setItems(Arrays.asList(person1, person2));
      table.setValue(List.of(person1));

      assertTrue(table.isSelected(person1));
      assertFalse(table.isSelected(person2));
    }

    @Test
    void shouldGetValue() {
      Table<Person> table = new Table<>();
      Person person1 = new Person("Fuck");
      Person person2 = new Person("Jane");

      table.setItems(Arrays.asList(person1, person2));
      table.setValue(List.of(person1));

      assertIterableEquals(new HashSet<>(Arrays.asList(person1)), table.getValue());
    }
  }

  @Nested
  class GetCellValueApi {

    @Test
    void shouldGetCellValueWithRowAndColumn() {
      String columnId = "testColumn";
      Function<String, String> provider = Function.identity();
      Column<String, String> column = component.addColumn(columnId, provider);
      component.setItems(Arrays.asList("testRow"));

      Object cellValue = component.getCellValue("testRow", column);

      assertNotNull(cellValue);
      assertEquals("testRow", cellValue);
    }
  }

  @Nested
  @DisplayName("Events API")
  @SuppressWarnings("rawtypes")
  class EventsApi {

    @Test
    void shouldAddRowClickListener() {
      component.onItemClick(event -> {
      });

      List<EventListener<TableItemClickEvent>> listeners =
          component.getEventListeners(TableItemClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemClickEvent>);
    }

    @Test
    void shouldAddRowDoubleClickListener() {
      component.onItemDoubleClick(event -> {
      });

      List<EventListener<TableItemDoubleClickEvent>> listeners =
          component.getEventListeners(TableItemDoubleClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemDoubleClickEvent>);
    }

    @Test
    void shouldAddCellClickListener() {
      component.onCellClick(event -> {
      });

      List<EventListener<TableCellClickEvent>> listeners =
          component.getEventListeners(TableCellClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableCellClickEvent>);
    }

    @Test
    void shouldAddCellDoubleClickListener() {
      component.onCellDoubleClick(event -> {
      });

      List<EventListener<TableCellDoubleClickEvent>> listeners =
          component.getEventListeners(TableCellDoubleClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableCellDoubleClickEvent>);
    }

    @Test
    void shouldAddRowSelectListener() {
      component.onItemSelect(event -> {
      });

      List<EventListener<TableItemSelectEvent>> listeners =
          component.getEventListeners(TableItemSelectEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemSelectEvent>);
    }

    @Test
    void shouldAddRowDeselectListener() {
      component.onItemDeselect(event -> {
      });

      List<EventListener<TableItemDeselectEvent>> listeners =
          component.getEventListeners(TableItemDeselectEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemDeselectEvent>);
    }

    @Test
    void shouldAddSelectionChangeListener() {
      component.onItemSelectionChange(event -> {
      });

      List<EventListener<TableItemSelectionChange>> listeners =
          component.getEventListeners(TableItemSelectionChange.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemSelectionChange>);
    }

    @Test
    void shouldAddValueChangeListener() {
      component.onValueChange(event -> {
      });

      List<EventListener<TableItemSelectionChange>> listeners =
          component.getEventListeners(TableItemSelectionChange.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemSelectionChange>);
    }

    @Test
    void shouldAddSortChangeListener() {
      component.onSortChange(event -> {
      });

      List<EventListener<TableSortChangeEvent>> listeners =
          component.getEventListeners(TableSortChangeEvent.class);

      assertTrue(listeners.size() > 0);
      assertTrue(listeners.get(0) instanceof EventListener<TableSortChangeEvent>);
    }

    @Test
    void shouldAddColumnResizeListener() {
      component.onColumnResize(event -> {
      });

      List<EventListener<TableColumnResizeEvent>> listeners =
          component.getEventListeners(TableColumnResizeEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableColumnResizeEvent>);
    }

    @Test
    void shouldAddColumnMoveListener() {
      component.onColumnMove(event -> {
      });

      List<EventListener<TableColumnMoveEvent>> listeners =
          component.getEventListeners(TableColumnMoveEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableColumnMoveEvent>);
    }
  }

  @Nested
  class Sorting {

    @Test
    void shouldHandleSortChangeEvent() {
      component.addColumn("col1", Function.identity());
      component.addColumn("col2", Function.identity());
      component.addColumn("col3", Function.identity()).setSortDirection(Column.SortDirection.DESC);

      Map<String, Object> payload = new HashMap<>();
      payload.put("criteria", """
          [
            {
              "id": "col1",
              "sort": "asc",
              "sortIndex": 1
            },
            {
              "id": "col2",
              "sort": "asc",
              "sortIndex": 2
            },
            {
              "id": "col3",
              "sort": "desc",
              "sortIndex": 3
            }
          ]
          """);
      TableSortChangeEvent<String> event = new TableSortChangeEvent<>(component, payload);

      component.handleSortChanged(event);

      // check columns directions are updated
      assertEquals(Column.SortDirection.ASC, component.getColumnById("col1").getSortDirection());
      assertEquals(Column.SortDirection.ASC, component.getColumnById("col2").getSortDirection());
      assertEquals(Column.SortDirection.DESC, component.getColumnById("col3").getSortDirection());
    }

    @Test
    void shouldSortDataOnServer() {
      Table<Person> table = spy(new Table<>());
      Element elMock = mock(Element.class);
      when(table.el()).thenReturn(elMock);
      when(elMock.isDestroyed()).thenReturn(true);
      when(elMock.isDefined()).thenReturn(true);

      table.addColumn("name", Person::getName).setSortDirection(Column.SortDirection.ASC);
      table.setItems(Arrays.asList(new Person("John"), new Person("Jane")));

      List<Person> sortedData = table.getRepository().findAll().toList();

      assertNotNull(sortedData);
      assertEquals(2, sortedData.size());
      assertEquals("Jane", sortedData.get(0).getName());
      assertEquals("John", sortedData.get(1).getName());
    }
  }

  @Nested
  class RowAndCellPartsApi {

    @Test
    void shouldSetAndGetRowPartProvider() {
      Function<String, List<String>> rowPartProvider = row -> List.of("part1", "part2");
      component.setRowPartProvider(rowPartProvider);

      assertEquals(rowPartProvider, component.getRowPartProvider());
    }

    @Test
    void shouldSetAndGetCellPartProvider() {
      BiFunction<String, Column<String, ?>, List<String>> cellPartProvider =
          (row, column) -> List.of("cellPart1", "cellPart2");
      component.setCellPartProvider(cellPartProvider);

      assertEquals(cellPartProvider, component.getCellPartProvider());
    }
  }

  @Nested
  class StateChangeHandling {

    @Test
    void shouldHandleColumnStateChange() {
      Column<String, String> column = component.addColumn("testColumn", Function.identity());

      column.setWidth(100.0f);
      column.setFlex(2);

      ColumnState state = new ColumnState();
      state.setId("testColumn");
      state.setWidth(250);
      state.setFlex(5);

      StateChangedDetail detail = new StateChangedDetail("user", List.of(state));
      component.handleStateChanged(detail);

      assertEquals(250.0f, column.getWidth());
      assertEquals(5, column.getFlex());
    }
  }

  @Nested
  class AutoSizeApi {

    @Test
    void shouldSetColumnsToAutoSize() {
      Table<String> table = spy(new Table<>());
      Element elMock = mock(Element.class);
      when(table.el()).thenReturn(elMock);
      when(elMock.callJsFunctionAsync("autoSize")).thenReturn(PendingResult.completedWith(null));

      PendingResult<Void> result = table.setColumnsToAutoSize();

      verify(elMock).callJsFunctionAsync("autoSize");
      assertNotNull(result);
    }

    @Test
    void shouldSetColumnToAutoSizeById() {
      Table<String> table = spy(new Table<>());
      Element elMock = mock(Element.class);
      when(table.el()).thenReturn(elMock);
      when(elMock.callJsFunctionAsync("autoSizeColumn", "testColumn"))
          .thenReturn(PendingResult.completedWith(null));

      PendingResult<Void> result = table.setColumnToAutoSize("testColumn");

      verify(elMock).callJsFunctionAsync("autoSizeColumn", "testColumn");
      assertNotNull(result);
    }

    @Test
    void shouldThrowNullPointerExceptionForNullColumnId() {
      Table<String> table = new Table<>();

      assertThrows(NullPointerException.class, () -> table.setColumnToAutoSize((String) null),
          "Column id cannot be null");
    }

    @Test
    void shouldSetColumnToAutoSizeByColumn() {
      Table<String> table = spy(new Table<>());
      Element elMock = mock(Element.class);
      when(table.el()).thenReturn(elMock);
      when(elMock.callJsFunctionAsync("autoSizeColumn", "testColumn"))
          .thenReturn(PendingResult.completedWith(null));
      Column<String, String> column = table.addColumn("testColumn", Function.identity());

      PendingResult<Void> result = table.setColumnToAutoSize(column);

      verify(elMock).callJsFunctionAsync("autoSizeColumn", "testColumn");
      assertNotNull(result);
    }

    @Test
    void shouldThrowNullPointerExceptionForNullColumn() {
      Table<String> table = new Table<>();

      assertThrows(NullPointerException.class,
          () -> table.setColumnToAutoSize((Column<String, ?>) null), "Column cannot be null");
    }

    @Test
    void shouldSetColumnsToAutoFit() {
      Table<String> table = spy(new Table<>());
      Element elMock = mock(Element.class);
      when(table.el()).thenReturn(elMock);
      when(elMock.callJsFunctionAsync("autoFit")).thenReturn(PendingResult.completedWith(null));

      PendingResult<Void> result = table.setColumnsToAutoFit();

      verify(elMock).callJsFunctionAsync("autoFit");
      assertNotNull(result);
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetColumnsToResizable(boolean resizable) {
      Table<String> table = spy(new Table<>());
      Column<String, String> column1 = table.addColumn("col1", Function.identity());
      Column<String, String> column2 = table.addColumn("col2", Function.identity());

      column1.setResizable(!resizable);
      column2.setResizable(!resizable);

      Table<String> result = table.setColumnsToResizable(resizable);

      assertEquals(resizable, column1.isResizable());
      assertEquals(resizable, column2.isResizable());
      assertEquals(table, result);
      verify(table).refreshColumns();
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetColumnsToMovable(boolean movable) {
      Table<String> table = spy(new Table<>());
      Column<String, String> column1 = table.addColumn("col1", Function.identity());
      Column<String, String> column2 = table.addColumn("col2", Function.identity());

      column1.setMovable(!movable);
      column2.setMovable(!movable);

      Table<String> result = table.setColumnsToMovable(movable);

      assertEquals(movable, column1.isMovable());
      assertEquals(movable, column2.isMovable());
      assertEquals(table, result);
      verify(table).refreshColumns();
    }
  }

  @Nested
  @DisplayName("Repository Key Conversion")
  class RepositoryKeyConversion {

    @Test
    @DisplayName("getSelectedKeys should return original repository keys, not string keys")
    void shouldReturnOriginalRepositoryKeysNotStringKeys() {
      // This test verifies the fix for the Spring Data issue where
      // getSelectedKeys() was returning String keys instead of the original
      // repository keys (e.g., Long), causing InvalidDataAccessApiUsageException
      // https://github.com/webforj/webforj/issues/1067


      // Create test entities with Long IDs
      EntityWithLongId entity1 = new EntityWithLongId(123L, "Entity 1");
      EntityWithLongId entity2 = new EntityWithLongId(456L, "Entity 2");

      // Set up a mock repository
      Repository<EntityWithLongId> repository = mock(Repository.class);
      when(repository.getKey(entity1)).thenReturn(123L);
      when(repository.getKey(entity2)).thenReturn(456L);
      when(repository.find(123L)).thenReturn(java.util.Optional.of(entity1));
      when(repository.find(456L)).thenReturn(java.util.Optional.of(entity2));
      // Add findByKey support for mapKeys method
      when(repository.findByKey(123L)).thenReturn(java.util.Optional.of(entity1));
      when(repository.findByKey(456L)).thenReturn(java.util.Optional.of(entity2));

      Table<EntityWithLongId> table = new Table<>();
      table.setRepository(repository);

      // Register entities in the internal registry (simulating data load)
      table.getItemKeysRegistry().getKey(entity1);
      table.getItemKeysRegistry().getKey(entity2);

      // Select using repository keys (simulating user selection)
      table.selectKey(123L, 456L);

      // Get selected keys - should return Long keys, not String keys
      List<Object> selectedKeys = table.getSelectedKeys();

      assertNotNull(selectedKeys);
      assertEquals(2, selectedKeys.size());

      // Verify we get Long keys back, not String keys
      assertTrue(selectedKeys.contains(123L));
      assertTrue(selectedKeys.contains(456L));
      assertFalse(selectedKeys.contains("123"));
      assertFalse(selectedKeys.contains("456"));

      // Verify getSelectedKey returns the correct type
      Object firstKey = table.getSelectedKey();
      assertNotNull(firstKey);
      assertEquals(Long.class, firstKey.getClass());
      assertTrue(firstKey.equals(123L) || firstKey.equals(456L));

      // Verify that getSelected() works correctly with the repository
      // This would previously fail with InvalidDataAccessApiUsageException
      // because it was passing String "123" to repository.find() instead of Long 123L
      EntityWithLongId selected = table.getSelected();
      assertNotNull(selected);
      assertTrue(selected.equals(entity1) || selected.equals(entity2));

      // Verify getSelectedItems() also works correctly
      List<EntityWithLongId> selectedItems = table.getSelectedItems();
      assertNotNull(selectedItems);
      assertEquals(2, selectedItems.size());
      assertTrue(selectedItems.contains(entity1));
      assertTrue(selectedItems.contains(entity2));
    }
  }

  static class EntityWithLongId implements HasEntityKey {
    private final Long id;
    private final String name;

    public EntityWithLongId(Long id, String name) {
      this.id = id;
      this.name = name;
    }

    public Long getId() {
      return id;
    }

    public String getName() {
      return name;
    }

    @Override
    public Object getEntityKey() {
      return id;
    }
  }

  class Person implements HasEntityKey {
    private transient String name;

    public Person(String name) {
      this.name = name;
    }

    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }

    @Override
    public String getEntityKey() {
      return String.valueOf("person-" + name);
    }
  }
}
