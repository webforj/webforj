package com.webforj.component.table;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.table.Column.PinDirection;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class ColumnGroupTest {

  @Test
  void shouldBuildGroupWithPropertiesAndChildren() {
    ColumnGroup child = ColumnGroup.of("child").add("c");
    ColumnGroup group = ColumnGroup.of("g1").setLabel("Group").setPinDirection(PinDirection.LEFT)
        .add("a").add("b").add(child);

    assertEquals("g1", group.getId());
    assertEquals("Group", group.getLabel());
    assertEquals("child", child.getLabel());
    assertEquals(PinDirection.LEFT, group.getPinDirection());
    assertEquals(3, group.getChildren().size());
    assertSame(child, group.getChildren().get(2));
  }

  @Test
  void shouldCollectLeafIdsFromNestedTree() {
    ColumnGroup group =
        ColumnGroup.of("parent").add("a").add(ColumnGroup.of("child").add("b").add("c"));
    List<String> ids = new ArrayList<>();
    group.collectLeafColumnIds(ids);
    assertEquals(List.of("a", "b", "c"), ids);
  }


  @Test
  void shouldFindImmediateParent() {
    ColumnGroup child = ColumnGroup.of("child").add("b");
    ColumnGroup parent = ColumnGroup.of("parent").add("a").add(child);

    assertSame(parent, ColumnGroup.findParent("a", List.of(parent)));
    assertSame(child, ColumnGroup.findParent("b", List.of(parent)));
  }

  @Test
  void shouldPreserveInstancesAndUpdateProperties() {
    ColumnGroup child = ColumnGroup.of("child").setLabel("Old").add("b");
    ColumnGroup parent = ColumnGroup.of("parent").add("a").add(child);

    ColumnGroup inChild = ColumnGroup.of("child").setLabel("New").add("b");
    ColumnGroup inParent =
        ColumnGroup.of("parent").setPinDirection(PinDirection.RIGHT).add(inChild).add("a");

    List<ColumnGroup> result = ColumnGroup.reconcile(List.of(parent), List.of(inParent));

    // same instances reused
    assertSame(parent, result.get(0));
    assertSame(child, result.get(0).getChildren().get(0));

    // properties updated
    assertEquals(PinDirection.RIGHT, parent.getPinDirection());
    assertEquals("New", child.getLabel());

    // children reordered child before "a"
    assertEquals("a", parent.getChildren().get(1).getId());
  }

}
