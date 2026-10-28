package com.webforj.automation.see;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
import java.util.List;
import org.junit.jupiter.api.Test;

class SnapshotNodeTest {

  @Test
  void shouldTreatAnEmptyTextAsNone() {
    SnapshotNode node = createNode(new SnapshotOptions());

    node.setText("");

    assertNull(node.getText());
  }

  @Test
  void shouldRemoveAnAttributeSetToNothing() {
    SnapshotNode node = createNode(new SnapshotOptions());
    node.setAttribute("zone", "red").setAttribute("level", 3).addFlag("hot");

    node.setAttribute("zone", "").setAttribute("level", null).removeAttribute("hot");

    assertFalse(node.hasAttribute("zone"));
    assertFalse(node.hasAttribute("level"));
    assertFalse(node.hasAttribute("hot"));
  }

  @Test
  void shouldTellFlagFromValue() {
    SnapshotNode node = createNode(new SnapshotOptions());

    node.setAttribute("zone", "red").addFlag("hot");

    assertEquals("red", node.getAttribute("zone"));
    assertNull(node.getAttribute("hot"));
    assertTrue(node.hasAttribute("hot"));
    assertFalse(node.hasAttribute("cold"));
  }

  @Test
  void shouldReportTheOptionsOfTheDescription() {
    SnapshotNode node = createNode(new SnapshotOptions().setMaxWidth(4).setBrowserState(true));

    assertEquals(4, node.getMaxWidth());
    assertTrue(node.isBrowserState());
  }

  @Test
  void shouldAddTheStructureAndItemLinesBelowTheNode() {
    SnapshotNode node = createNode(new SnapshotOptions());

    SnapshotNode scale = node.addChild("Scale");
    SnapshotNode mark = node.addItem("Mark");

    assertEquals(List.of(scale), node.getStructure());
    assertEquals(List.of(mark), node.getItems());
    assertSame(node, scale.getParent());
    assertNull(mark.getComponent());
  }

  @Test
  void shouldRefuseNegativeItemCount() {
    SnapshotNode node = createNode(new SnapshotOptions());

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, () -> node.setItemCount(-1));

    assertEquals("The item count cannot be negative", failure.getMessage());
  }

  @Test
  void shouldDescribeAnAddedComponent() {
    SnapshotNode node = createNode(new SnapshotOptions());
    Button save = new Button("Save");

    SnapshotNode added = node.addChild(save);

    assertEquals(List.of(added), node.getStructure());
    assertEquals("Button", added.getType());
    assertEquals("Save", added.getText());
    assertSame(save, added.getComponent());
  }

  @Test
  void shouldLeaveAnAddedHiddenComponentOutOfTheTree() {
    SnapshotNode node = createNode(new SnapshotOptions());
    Button save = new Button("Save");
    save.setVisible(false);

    SnapshotNode added = node.addChild(save).setAttribute("slot", "prefix");

    assertEquals(List.of(), node.getStructure());
    assertSame(save, added.getComponent());
  }

  private static SnapshotNode createNode(SnapshotOptions options) {
    SnapshotRenderer renderer =
        new SnapshotRenderer(new SnapshotSerializerRegistry(List.of()), options);

    return renderer.describe(new FlexLayout());
  }
}
