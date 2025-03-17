package com.webforj.component.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.spy;

import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.table.event.TableSortChangeEvent;
import com.webforj.component.table.event.cell.TableCellClickEvent;
import com.webforj.component.table.event.cell.TableCellDoubleClickEvent;
import com.webforj.component.table.event.item.TableItemClickEvent;
import com.webforj.component.table.event.item.TableItemDoubleClickEvent;
import com.webforj.component.table.event.selection.TableItemDeselectEvent;
import com.webforj.component.table.event.selection.TableItemSelectEvent;
import com.webforj.component.table.event.selection.TableItemSelectionChange;
import com.webforj.component.table.renderer.Renderer;
import com.webforj.data.HasEntityKey;
import com.webforj.dispatcher.EventListener;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class TableTest {

  private Table<String> component;

  @BeforeEach
  void setUp() {
    component = new Table<>();
  }

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
  }

  @Nested
  class Sorting {

    @Test
    void shouldHandleSortChangeEvent() {
      component.addColumn("col1", Function.identity());
      component.addColumn("col2", Function.identity());
      component.addColumn("col3", Function.identity()).setSortDirection(Column.SortDirection.DESC);

      Map<String, Object> payload = new HashMap<>();
      payload.put("criteria", new HashMap<>() {
        {
          put("col1", "asc");
          put("col2", "desc");
        }
      });
      TableSortChangeEvent<String> event = new TableSortChangeEvent<>(component, payload);

      component.handleSortChanged(event);

      // check columns directions are updated
      assertEquals(Column.SortDirection.ASC, component.getColumnById("col1").getSortDirection());
      assertEquals(Column.SortDirection.DESC, component.getColumnById("col2").getSortDirection());
      assertEquals(Column.SortDirection.NONE, component.getColumnById("col3").getSortDirection());
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
