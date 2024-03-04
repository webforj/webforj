package com.webforj.addons.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import com.google.gson.Gson;
import com.webforj.addons.table.Column.PinDirection;
import com.webforj.addons.table.Column.SortDirection;
import com.webforj.addons.table.Column.Type;
import com.webforj.addons.table.renderer.Renderer;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class ColumnTest {
  private Column<String, String> column;
  private Map<String, Object> oldValues;
  private Map<String, Object> newValues;

  @BeforeEach
  void setUp() {
    column = new Column<>(new Table<>(), "test");
    oldValues = new HashMap<>();
    newValues = new HashMap<>();

    oldValues.put("hidden", column.isHidden());
    oldValues.put("label", column.getLabel());
    oldValues.put("pinDirection", column.getPinDirection());
    oldValues.put("sortable", column.isSortable());
    oldValues.put("sortDirection", column.getSortDirection());
    oldValues.put("suppressNavigable", column.isSuppressNavigable());
    oldValues.put("minWidth", column.getMinWidth());
    oldValues.put("alignment", column.getAlignment());

    newValues.put("hidden", true);
    newValues.put("label", "New Label");
    newValues.put("pinDirection", PinDirection.LEFT);
    newValues.put("sortable", false);
    newValues.put("sortDirection", SortDirection.ASC);
    newValues.put("suppressNavigable", true);
    newValues.put("type", Type.NUMBER);
    newValues.put("minWidth", 10.0f);
    newValues.put("alignment", Column.Alignment.RIGHT);
  }

  @Test
  void shouldInvokeChangeListeners() {
    assertEquals("test", column.getId());

    PropertyChangeListener listener = new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        assertEquals(oldValues.get(evt.getPropertyName()), evt.getOldValue());
        assertEquals(newValues.get(evt.getPropertyName()), evt.getNewValue());
      }
    };
    column.addPropertyChangeListener(listener);

    column.setHidden((boolean) newValues.get("hidden"));
    column.setLabel((String) newValues.get("label"));
    column.setPinDirection((PinDirection) newValues.get("pinDirection"));
    column.setSortable((boolean) newValues.get("sortable"));
    column.setSortDirection((SortDirection) newValues.get("sortDirection"));
    column.setSuppressNavigable((boolean) newValues.get("suppressNavigable"));
    column.setMinWidth((float) newValues.get("minWidth"));
    column.setAlignment((Column.Alignment) newValues.get("alignment"));
  }

  @Test
  void shouldBeSerialization() {
    Gson gson = new Gson();
    String json = gson.toJson(column);
    Column<String, String> deserializedColumn = gson.fromJson(json, Column.class);

    assertEquals(column.getId(), deserializedColumn.getId());
    assertEquals(column.isHidden(), deserializedColumn.isHidden());
    assertEquals(column.getLabel(), deserializedColumn.getLabel());
    assertEquals(column.getPinDirection(), deserializedColumn.getPinDirection());
    assertEquals(column.isSortable(), deserializedColumn.isSortable());
    assertEquals(column.getSortDirection(), deserializedColumn.getSortDirection());
    assertEquals(column.isSuppressNavigable(), deserializedColumn.isSuppressNavigable());
    assertEquals(column.getAlignment(), deserializedColumn.getAlignment());
    assertNull(column.getMinWidth());

  }

  @Test
  void shouldGetValue() {
    column.setValueProvider(value -> value.toUpperCase());
    assertEquals("HELLO", column.getValue("Hello"));
  }

  @Nested
  class RendererApi {
    Table<String> table;
    Column<String, String> spy;
    MockRenderer renderer;

    class MockRenderer extends Renderer<String> {
      @Override
      public String build() {
        return "";
      }
    }

    @BeforeEach
    void setUp() {
      table = mock(Table.class);
      spy = spy(new Column<>(table, "test"));

      renderer = new MockRenderer();
    }

    @Test
    void shouldConfigureRenderer() {
      spy.setRenderer(renderer);
      assertNotNull(spy.getRenderer());
    }

    @Test
    void shouldConfigureTableInstance() {
      assertNull(renderer.getTable());

      spy.setRenderer(renderer);
      assertEquals(spy.getTable(), renderer.getTable());
    }

    @Test
    void shouldNotAcceptAttachedRenderer() {
      Renderer<String> newRenderer = new MockRenderer();
      newRenderer.setTable(mock(Table.class));

      assertThrows(IllegalArgumentException.class, () -> spy.setRenderer(newRenderer));
    }
  }

  static Stream<Arguments> provideTypesForTest() {
    return Stream.of(Arguments.of(String.class, Type.TEXT),
        Arguments.of(Integer.class, Type.NUMBER), Arguments.of(Boolean.class, Type.BOOLEAN),
        Arguments.of(java.util.Date.class, Type.DATE), Arguments.of(java.sql.Date.class, Type.DATE),
        Arguments.of(java.time.LocalDate.class, Type.DATE),
        Arguments.of(java.time.LocalDateTime.class, Type.DATETIME),
        Arguments.of(java.time.ZonedDateTime.class, Type.DATETIME),
        Arguments.of(java.time.OffsetDateTime.class, Type.DATETIME),
        Arguments.of(java.sql.Timestamp.class, Type.DATETIME),
        Arguments.of(java.time.LocalTime.class, Type.TIME),
        Arguments.of(java.time.OffsetTime.class, Type.TIME));
  }

  @ParameterizedTest
  @MethodSource("provideTypesForTest")
  void shouldFigureClientType(Class<?> inputType, Type expectedType) {
    Column<String, String> column = new Column<>(new Table<>(), "test");
    column.figureClientType(inputType);
    assertEquals(expectedType, column.getClientType());
  }
}
