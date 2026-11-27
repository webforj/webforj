package com.webforj.automation.act.component.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.icons.TablerIcon;
import com.webforj.component.table.Column;
import com.webforj.component.table.Table;
import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.component.table.renderer.ButtonRenderer;
import com.webforj.component.table.renderer.CompositeRenderer;
import com.webforj.component.table.renderer.ConditionalRenderer;
import com.webforj.component.table.renderer.CurrencyRenderer;
import com.webforj.component.table.renderer.IconButtonRenderer;
import com.webforj.component.window.Frame;
import com.webforj.data.repository.OrderCriteria;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class TableInteractionTest {

  @Test
  void shouldReplaceTheSelectionOfTheTableThatSelectsOneRow() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = createTable("Ada", "Grace");
      table.setSelectionMode(Table.SelectionMode.SINGLE);
      List<String> events = new ArrayList<>();
      table.onItemDeselect(event -> events.add("deselect " + event.getItem()));
      table.onItemSelect(event -> events.add("select " + event.getItem()));
      table.onItemSelectionChange(event -> events.add("change " + event.getSelectedItems()));
      frame.add(table);

      UserInteraction.select(table, "Ada");
      UserInteraction.select(table, "Ada");
      UserInteraction.selectKey(table, "Grace");

      assertEquals(
          List.of("select Ada", "change [Ada]", "deselect Ada", "select Grace", "change [Grace]"),
          events);
      assertEquals(List.of("Grace"), table.getSelectedItems());
    }
  }

  @Test
  void shouldAddToAndTakeFromTheSelectionOfTheTableThatSelectsSeveralRows() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = createTable("Ada", "Grace");
      table.setSelectionMode(Table.SelectionMode.MULTIPLE);
      List<String> events = new ArrayList<>();
      table.onItemDeselect(event -> events.add("deselect " + event.getItem()));
      table.onItemSelectionChange(
          event -> events.add("change " + new TreeSet<>(event.getSelectedItems())));
      frame.add(table);

      UserInteraction.select(table, "Ada");
      UserInteraction.select(table, "Grace");
      UserInteraction.deselect(table, "Ada");
      UserInteraction.deselect(table, "Ada");

      assertEquals(List.of("change [Ada]", "change [Ada, Grace]", "deselect Ada", "change [Grace]"),
          events);
    }
  }

  @Test
  void shouldRejectTheSelectionTheTableDoesNotAllow() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> none = createTable("Ada");
      Table<String> fixed = createTable("Ada");
      fixed.setSelectionMode(Table.SelectionMode.MULTIPLE).setDeselection(false);
      frame.add(none, fixed);
      UserInteraction.select(fixed, "Ada");

      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.select(none, "Ada")).getMessage().contains("does not select rows"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.deselect(fixed, "Ada")).getMessage()
          .contains("does not deselect rows"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.select(fixed, "Linus")).getMessage().contains("Linus"));
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.selectKey(fixed, "Linus")).getMessage().contains("Linus"));
      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.select(fixed, 42))
              .getMessage().contains("42"));
    }
  }

  @Test
  void shouldReportTheCellAndTheRowOfTheClick() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = createTable("Ada", "Grace");
      List<String> events = new ArrayList<>();
      table.onCellClick(event -> events.add("cell " + event.getColumn().getId()));
      table.onItemClick(event -> events.add("row " + event.getItem() + " " + event.getItemIndex()));
      table.onCellDoubleClick(event -> events.add("cell twice " + event.getItem()));
      table.onItemDoubleClick(event -> events.add("row twice " + event.getItem()));
      frame.add(table);

      UserInteraction.click(table, "Grace");
      UserInteraction.doubleClick(table, "Ada");

      assertEquals(List.of("cell name", "row Grace 1", "cell twice Ada", "row twice Ada"), events);
      assertTrue(table.getSelectedItems().isEmpty());
    }
  }

  @Test
  void shouldReportTheSortedColumnsInTheirOrder() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = createTable("Ada");
      Column<String, ?> name = table.getColumns().get(0);
      Column<String, Integer> length = table.addColumn("length", String::length);
      name.setSortable(true);
      length.setSortable(true);
      List<Object> criteria = new ArrayList<>();
      table.onSortChange(event -> criteria.add(event.getClientCriterion()));
      frame.add(table);
      Map<Column<String, ?>, OrderCriteria.Direction> sorted = new LinkedHashMap<>();
      sorted.put(length, OrderCriteria.Direction.DESC);
      sorted.put(name, OrderCriteria.Direction.ASC);

      UserInteraction.perform(table, TableInteraction.sort(sorted));

      assertEquals("[{length=desc, name=asc}]", criteria.toString());
    }
  }

  @Test
  void shouldMoveAndResizeTheColumnsTheUserCanChange() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = createTable("Ada");
      Column<String, ?> name = table.getColumns().get(0);
      Column<String, Integer> length = table.addColumn("length", String::length);
      name.setWidth(120f);
      length.setSortable(false).setMovable(false).setResizable(false);
      List<String> events = new ArrayList<>();
      table.onColumnMove(event -> events.add(event.getColumn().getId() + " from "
          + event.getOldIndex() + " to " + event.getNewIndex()));
      table.onColumnResize(event -> events.add(event.getColumn().getId() + " from "
          + event.getOldWidth() + " to " + event.getNewWidth()));
      frame.add(table);

      UserInteraction.perform(table, TableInteraction.moveColumn(name, 1));
      UserInteraction.perform(table, TableInteraction.resizeColumn(name, 80f));

      assertEquals(List.of("name from 0 to 1", "name from 120.0 to 80.0"), events);
      assertEquals(80f, name.getWidth());
      Consumer<Table<String>> moveFixed = TableInteraction.moveColumn(length, 0);
      assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, moveFixed));
      Consumer<Table<String>> resizeFixed = TableInteraction.resizeColumn(length, 10f);
      assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, resizeFixed));
      Consumer<Table<String>> sortFixed =
          TableInteraction.sort(Map.of(length, OrderCriteria.Direction.ASC));
      assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, sortFixed));
      Column<String, ?> foreign = new Table<String>().addColumn("other", item -> item);
      Consumer<Table<String>> moveForeign = TableInteraction.moveColumn(foreign, 0);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, moveForeign)).getMessage().contains("other"));
    }
  }

  @Test
  void shouldDeliverClicksOnRenderedTableContent() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = new Table<>();
      List<String> clicked = new ArrayList<>();
      Column<String, String> action = table.addColumn("action",
          new ButtonRenderer<>("Go", event -> clicked.add(event.getItem())));
      table.setItems(List.of("Ada", "Linus"));
      frame.add(table);

      UserInteraction.fireEvent(new RendererClickEvent<>(table, Map.of("itemKey",
          table.getItemKeysRegistry().getKey("Linus"), "columnKey", action.getId())));

      assertEquals(List.of("Linus"), clicked);
    }
  }

  @Test
  void shouldClickTheRendererOfTheGivenRowAndColumn() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = new Table<>();
      table.setItems(List.of("Ada", "Linus"));
      List<String> clicked = new ArrayList<>();
      Column<String, String> edit = table.addColumn("edit",
          new ButtonRenderer<>("Edit", event -> clicked.add("edit " + event.getItem())));
      Column<String, String> delete = table.addColumn("delete",
          new ButtonRenderer<>("Delete", event -> clicked.add("delete " + event.getItem())));
      frame.add(table);

      UserInteraction.perform(table, TableInteraction.clickRenderer("Linus", delete));
      UserInteraction.perform(table, TableInteraction.clickRenderer("Ada", edit));

      assertEquals(List.of("delete Linus", "edit Ada"), clicked);
    }
  }

  @Test
  void shouldRejectTheRendererClickTheTableDoesNotOffer() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = createTable("Ada");
      Column<String, ?> name = table.getColumns().get(0);
      Column<String, String> action =
          table.addColumn("action", new ButtonRenderer<>("Go", event -> {
          }));
      frame.add(table);

      Consumer<Table<String>> plainColumn = TableInteraction.clickRenderer("Ada", name);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, plainColumn)).getMessage()
          .contains("holds the column 'name' without a renderer"));
      Consumer<Table<String>> missingItem = TableInteraction.clickRenderer("Linus", action);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, missingItem)).getMessage().contains("Linus"));
      Column<String, ?> foreign = new Table<String>().addColumn("other", item -> item);
      Consumer<Table<String>> foreignColumn = TableInteraction.clickRenderer("Ada", foreign);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, foreignColumn)).getMessage().contains("other"));
    }
  }

  @Test
  void shouldSelectTableRowsByItemAndByKey() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = new Table<>();
      table.addColumn("name", item -> item);
      table.setItems(List.of("Ada", "Grace", "Linus"));
      table.setSelectionMode(Table.SelectionMode.MULTIPLE);
      List<String> selections = new ArrayList<>();
      table.onItemSelect(event -> selections.add(event.getItem()));
      frame.add(table);

      UserInteraction.select(table, "Grace");
      UserInteraction.selectKey(table, "Ada");

      assertEquals(List.of("Grace", "Ada"), selections);
      assertTrue(table.getSelectedItems().containsAll(List.of("Grace", "Ada")));
    }
  }

  @Test
  void shouldClickTheRendererNestedInTheRendererOfTheColumn() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = new Table<>();
      table.setItems(List.of("Ada", "Linus"));
      List<String> clicked = new ArrayList<>();
      ButtonRenderer<String> edit =
          new ButtonRenderer<>("Edit", event -> clicked.add("edit " + event.getItem()));
      ButtonRenderer<String> delete =
          new ButtonRenderer<>("Delete", event -> clicked.add("delete " + event.getItem()));
      ButtonRenderer<String> open =
          new ButtonRenderer<>("Open", event -> clicked.add("open " + event.getItem()));
      Column<String, String> actions =
          table.addColumn("actions", new CompositeRenderer<>(edit, delete));
      Column<String, String> state =
          table.addColumn("state", new ConditionalRenderer<String>().when("Ada", open));
      frame.add(table);

      UserInteraction.perform(table, TableInteraction.clickRenderer("Ada", state, open));
      UserInteraction.perform(table, TableInteraction.clickRenderer("Linus", actions, delete));
      UserInteraction.perform(table, TableInteraction.clickRenderer("Ada", actions, edit));

      assertEquals(List.of("open Ada", "delete Linus", "edit Ada"), clicked);
    }
  }

  @Test
  void shouldRejectTheNestedRendererClickTheUserCannotMake() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = createTable("Ada");
      ButtonRenderer<String> edit = new ButtonRenderer<>("Edit");
      ButtonRenderer<String> delete = new ButtonRenderer<>("Delete");
      delete.setEnabled(false);
      ButtonRenderer<String> other = new ButtonRenderer<>("Other");
      Column<String, String> actions =
          table.addColumn("actions", new CompositeRenderer<>(edit, delete));
      frame.add(table);

      Consumer<Table<String>> foreign = TableInteraction.clickRenderer("Ada", actions, other);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, foreign)).getMessage()
          .contains("holds the column 'actions' without the given renderer"));
      Consumer<Table<String>> disabled = TableInteraction.clickRenderer("Ada", actions, delete);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, disabled)).getMessage()
          .contains("holds the column 'actions' with a disabled renderer"));
      ButtonRenderer<String> elsewhere = new ButtonRenderer<>("Elsewhere");
      new Table<String>().addColumn("elsewhere", elsewhere);
      CompositeRenderer<String> holder = new CompositeRenderer<>();
      Column<String, String> shared = table.addColumn("shared", holder);
      holder.add(elsewhere);
      Consumer<Table<String>> attached = TableInteraction.clickRenderer("Ada", shared, elsewhere);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, attached)).getMessage()
          .contains("holds the column 'shared' with a renderer of another table"));
    }
  }

  @ParameterizedTest
  @MethodSource("createRendererClicksTheUserCannotMake")
  void shouldRejectTheRendererClickTheUserCannotMake(
      Function<Table<String>, Column<String, ?>> columns, String message) throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Table<String> table = createTable("Ada");
      Column<String, ?> column = columns.apply(table);
      frame.add(table);

      Consumer<Table<String>> click = TableInteraction.clickRenderer("Ada", column);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(table, click)).getMessage().contains(message));
    }
  }

  private static Stream<Arguments> createRendererClicksTheUserCannotMake() {
    Function<Table<String>, Column<String, ?>> currency = table -> table
        .addColumn("price", item -> item).setRenderer(new CurrencyRenderer<>(Locale.US));
    Function<Table<String>, Column<String, ?>> conditional =
        table -> table.addColumn("state", item -> item)
            .setRenderer(new ConditionalRenderer<String>().otherwise(new ButtonRenderer<>("Go")));
    Function<Table<String>, Column<String, ?>> disabledButton =
        table -> table.addColumn("action", new ButtonRenderer<String>("Go").setEnabled(false));
    Function<Table<String>, Column<String, ?>> disabledIconButton =
        table -> table.addColumn("remove",
            new IconButtonRenderer<String>(TablerIcon.create("trash")).setEnabled(false));
    Function<Table<String>, Column<String, ?>> hidden =
        table -> table.addColumn("action", new ButtonRenderer<String>("Go")).setHidden(true);
    Function<Table<String>, Column<String, ?>> duplicate = table -> {
      table.addColumn("action", new ButtonRenderer<String>("Go"));

      return table.addColumn("action", new ButtonRenderer<String>("Go"));
    };

    return Stream.of(
        Arguments.of(currency, "holds the column 'price' without a renderer a user can click"),
        Arguments.of(conditional, "holds the column 'state' without a renderer a user can click"),
        Arguments.of(disabledButton, "holds the column 'action' with a disabled renderer"),
        Arguments.of(disabledIconButton, "holds the column 'remove' with a disabled renderer"),
        Arguments.of(hidden, "holds the column 'action' hidden"),
        Arguments.of(duplicate, "holds more than one column with the id 'action'"));
  }

  private static Table<String> createTable(String... items) {
    Table<String> table = new Table<>();
    table.addColumn("name", item -> item);
    table.setItems(List.of(items));

    return table;
  }
}
