package com.webforj.component.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import com.google.gson.Gson;
import com.webforj.component.table.Column.PinDirection;
import com.webforj.component.table.Column.SortDirection;
import com.webforj.component.table.Column.Type;
import com.webforj.component.table.renderer.Renderer;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
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
    oldValues.put("minWidth", column.getMinWidth() == 0 ? null : column.getMinWidth());
    oldValues.put("maxWidth", column.getMaxWidth() == 0 ? null : column.getMaxWidth());
    oldValues.put("alignment", column.getAlignment());
    oldValues.put("width", column.getWidth() == 0 ? null : column.getWidth());
    oldValues.put("flex", column.getFlex());
    oldValues.put("resizable", column.isResizable());

    newValues.put("hidden", true);
    newValues.put("label", "New Label");
    newValues.put("pinDirection", PinDirection.LEFT);
    newValues.put("sortable", false);
    newValues.put("sortDirection", SortDirection.ASC);
    newValues.put("suppressNavigable", true);
    newValues.put("type", Type.NUMBER);
    newValues.put("minWidth", 10.0f);
    newValues.put("maxWidth", 500.0f);
    newValues.put("alignment", Column.Alignment.RIGHT);
    newValues.put("width", 250.0f);
    newValues.put("flex", 3.0f);
    newValues.put("resizable", false);
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
    column.setMaxWidth((float) newValues.get("maxWidth"));
    column.setAlignment((Column.Alignment) newValues.get("alignment"));
    column.setWidth((float) newValues.get("width"));
    column.setFlex((float) newValues.get("flex"));
    column.setResizable((boolean) newValues.get("resizable"));
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
    assertEquals(0, column.getMinWidth(), 0.01);

  }

  @Test
  void shouldGetValue() {
    column.setValueProvider(String::toUpperCase);
    assertEquals("HELLO", column.getValue("Hello"));
  }

  @Test
  void shouldValidatePositiveWidth() {
    assertThrows(IllegalArgumentException.class, () -> column.setWidth(-10));
    assertThrows(IllegalArgumentException.class, () -> column.setMinWidth(-5));
    assertThrows(IllegalArgumentException.class, () -> column.setMaxWidth(-100));
  }

  @Test
  void shouldValidateNonNegativeFlex() {
    // 0 is valid (means no flex)
    column.setFlex(0);
    assertEquals(0, column.getFlex());

    // Positive values are valid
    column.setFlex(1.5f);
    assertEquals(1.5f, column.getFlex());

    // Negative values should throw
    assertThrows(IllegalArgumentException.class, () -> column.setFlex(-1));
    assertThrows(IllegalArgumentException.class, () -> column.setFlex(-0.5f));
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
    Column<String, String> localColumn = new Column<>(new Table<>(), "test");
    localColumn.figureClientType(inputType);
    assertEquals(expectedType, localColumn.getClientType());
  }
}
