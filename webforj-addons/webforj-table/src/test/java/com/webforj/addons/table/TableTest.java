package com.webforj.addons.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.spy;

import com.webforj.addons.table.event.TableSortChangeEvent;
import com.webforj.addons.table.event.cell.TableCellClickEvent;
import com.webforj.addons.table.event.cell.TableCellDoubleClickEvent;
import com.webforj.addons.table.event.item.TableItemClickEvent;
import com.webforj.addons.table.event.item.TableItemDoubleClickEvent;
import com.webforj.addons.table.event.selection.TableItemDeselectEvent;
import com.webforj.addons.table.event.selection.TableItemSelectEvent;
import com.webforj.addons.table.event.selection.TableItemSelectionChange;
import com.webforj.addons.table.renderer.Renderer;
import com.webforj.component.element.PropertyDescriptorTester;
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

  private Table<String> table;

  @BeforeEach
  void setUp() {
    table = new Table<>();
  }

  @Test
  void shouldSetGetProperties() {
    try {
      PropertyDescriptorTester.run(Table.class, table, descriptor -> {
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
      Column<String, String> column = table.addColumn(columnId, provider);

      assertNotNull(column);
      assertEquals(columnId, column.getId());
      assertEquals(provider, column.getValueProvider());
    }

    @Test
    void shouldAddColumnWithOnlyValueProvider() {
      Function<String, String> provider = Function.identity();
      Column<String, String> column = table.addColumn(provider);

      assertNotNull(column);
      assertNotNull(column.getId());
      assertEquals(provider, column.getValueProvider());
    }

    @Test
    void shouldAddColumnWithRenderer() {
      Renderer<String> renderer = spy(Renderer.class);
      Column<String, String> column = table.addColumn(renderer);

      assertNotNull(column);
      assertNotNull(column.getId());
      assertNotNull(column.getValueProvider());
      assertEquals("", column.getLabel());
      assertEquals(renderer, column.getRenderer());
    }

    @Test
    void shouldGetColumnById() {
      String columnId = "testColumn";
      table.addColumn(columnId, Function.identity());
      Column<String, ?> foundColumn = table.getColumnById(columnId);

      assertNotNull(foundColumn);
      assertEquals(columnId, foundColumn.getId());
    }

    @Test
    void getColumnByIdShouldReturnNullIfColumnDoesNotExist() {
      String nonexistentId = "nonexistentId";
      Column<String, ?> foundColumn = table.getColumnById(nonexistentId);

      assertNull(foundColumn);
    }

    @Test
    void shouldCheckIfColumnExists() {
      String columnId = "testColumn";
      table.addColumn(columnId, Function.identity());

      assertTrue(table.hasColumn(columnId));
      assertFalse(table.hasColumn("nonexistentId"));
    }

    @Test
    void shouldGetUnmodifiableColumns() {
      String columnId = "testColumn";
      table.addColumn(columnId, Function.identity());

      List<Column<String, ?>> columns = table.getColumns();

      assertThrows(UnsupportedOperationException.class, () -> columns.add(null));
    }

    @Test
    void shouldRemoveColumn() {
      String columnId = "testColumn";
      Column<String, ?> c = table.addColumn(columnId, Function.identity());
      table.removeColumn(c);

      assertFalse(table.hasColumn(columnId));
    }
  }

  @Nested
  class RowsApi {
    @Test
    void shouldSetItems() {
      List<String> items = Arrays.asList("item1", "item2");
      table.setItems(items);

      assertIterableEquals(items, table.getItems());
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
  }

  @Nested
  class GetCellValueApi {

    @Test
    void shouldGetCellValueWithRowAndColumn() {
      String columnId = "testColumn";
      Function<String, String> provider = Function.identity();
      Column<String, String> column = table.addColumn(columnId, provider);
      table.setItems(Arrays.asList("testRow"));

      Object cellValue = table.getCellValue("testRow", column);

      assertNotNull(cellValue);
      assertEquals("testRow", cellValue);
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddRowClickListener() {
      table.onItemClick(event -> {
      });

      List<EventListener<TableItemClickEvent>> listeners =
          table.getEventListeners(TableItemClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemClickEvent>);
    }

    @Test
    void shouldAddRowDoubleClickListener() {
      table.onItemDoubleClick(event -> {
      });

      List<EventListener<TableItemDoubleClickEvent>> listeners =
          table.getEventListeners(TableItemDoubleClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemDoubleClickEvent>);
    }

    @Test
    void shouldAddCellClickListener() {
      table.onCellClick(event -> {
      });

      List<EventListener<TableCellClickEvent>> listeners =
          table.getEventListeners(TableCellClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableCellClickEvent>);
    }

    @Test
    void shouldAddCellDoubleClickListener() {
      table.onCellDoubleClick(event -> {
      });

      List<EventListener<TableCellDoubleClickEvent>> listeners =
          table.getEventListeners(TableCellDoubleClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableCellDoubleClickEvent>);
    }

    @Test
    void shouldAddRowSelectListener() {
      table.onItemSelect(event -> {
      });

      List<EventListener<TableItemSelectEvent>> listeners =
          table.getEventListeners(TableItemSelectEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemSelectEvent>);
    }

    @Test
    void shouldAddRowDeselectListener() {
      table.onItemDeselect(event -> {
      });

      List<EventListener<TableItemDeselectEvent>> listeners =
          table.getEventListeners(TableItemDeselectEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemDeselectEvent>);
    }

    @Test
    void shouldAddSelectionChangeListener() {
      table.onItemSelectionChange(event -> {
      });

      List<EventListener<TableItemSelectionChange>> listeners =
          table.getEventListeners(TableItemSelectionChange.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<TableItemSelectionChange>);
    }

    @Test
    void shouldAddSortChangeListener() {
      table.onSortChange(event -> {
      });

      List<EventListener<TableSortChangeEvent>> listeners =
          table.getEventListeners(TableSortChangeEvent.class);

      assertTrue(listeners.size() > 0);
      assertTrue(listeners.get(0) instanceof EventListener<TableSortChangeEvent>);
    }
  }

  @Nested
  class Sorting {

    @Test
    void shouldHandleSortChangeEvent() {
      table.addColumn("col1", Function.identity());
      table.addColumn("col2", Function.identity());
      table.addColumn("col3", Function.identity()).setSortDirection(Column.SortDirection.DESC);

      Map<String, Object> payload = new HashMap<>();
      payload.put("criteria", new HashMap<>() {
        {
          put("col1", "asc");
          put("col2", "desc");
        }
      });
      TableSortChangeEvent<String> event = new TableSortChangeEvent<>(table, payload);

      table.handleSortChanged(event);

      // check columns directions are updated
      assertEquals(Column.SortDirection.ASC, table.getColumnById("col1").getSortDirection());
      assertEquals(Column.SortDirection.DESC, table.getColumnById("col2").getSortDirection());
      assertEquals(Column.SortDirection.NONE, table.getColumnById("col3").getSortDirection());
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
