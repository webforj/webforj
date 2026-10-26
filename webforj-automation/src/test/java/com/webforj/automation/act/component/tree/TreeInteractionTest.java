package com.webforj.automation.act.component.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class TreeInteractionTest {

  @Test
  void shouldExpandCollapseAndClickTheNodesOfTheTree() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Tree tree = new Tree();
      TreeNode fruits = Tree.node("fruits", "Fruits");
      TreeNode apple = Tree.node("apple", "Apple");
      fruits.add(apple);
      tree.add(fruits);
      List<String> events = new ArrayList<>();
      tree.onExpand(event -> events
          .add("expand " + event.getNode().getText() + " " + tree.isExpanded(event.getNode())));
      tree.onCollapse(event -> events
          .add("collapse " + event.getNode().getText() + " " + tree.isExpanded(event.getNode())));
      tree.onClick(event -> events.add("click " + event.getNode().getText() + " " + event.getX()));
      tree.onDoubleClick(event -> events.add("twice " + event.getNode().getText()));
      tree.onSelect(
          event -> events.add("select " + event.getNode().getText() + " " + tree.getSelectedKey()));
      tree.onDeselect(event -> events
          .add("deselect " + event.getNode().getText() + " " + tree.getSelectedKey()));
      frame.add(tree);

      UserInteraction.perform(tree, TreeInteraction.expand(fruits));
      UserInteraction.perform(tree, TreeInteraction.expand(fruits));
      UserInteraction.click(tree, apple, new MouseOptions().setX(5));
      UserInteraction.doubleClick(tree, apple);
      UserInteraction.select(tree, apple);
      UserInteraction.deselect(tree, apple);
      UserInteraction.perform(tree, TreeInteraction.collapse(fruits));
      UserInteraction.perform(tree, TreeInteraction.collapse(fruits));

      assertEquals(List.of("expand Fruits true", "click Apple 5", "select Apple apple",
          "twice Apple", "select Apple apple", "deselect Apple null", "collapse Fruits false"),
          events);
      TreeNode foreign = Tree.node("plum", "Plum");
      Consumer<Tree> expandForeign = TreeInteraction.expand(foreign);
      assertTrue(assertThrows(InteractionRejectedException.class,
          () -> UserInteraction.perform(tree, expandForeign)).getMessage().contains("Plum"));
    }
  }

  @Test
  void shouldRejectClickingTheNodeUnderTheCollapsedParent() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Tree tree = new Tree();
      TreeNode fruits = Tree.node("fruits", "Fruits");
      TreeNode apple = Tree.node("apple", "Apple");
      fruits.add(apple);
      tree.add(fruits);
      List<String> events = new ArrayList<>();
      tree.onClick(event -> events.add("click"));
      frame.add(tree);

      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.click(tree, apple))
              .getMessage().contains("collapsed"));
      assertEquals(List.of(), events);
    }
  }

  @Test
  void shouldDeselectTheSelectedNodeBeforeSelectingAnotherOneInSingleMode() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Tree tree = new Tree();
      TreeNode apple = Tree.node("apple", "Apple");
      TreeNode pear = Tree.node("pear", "Pear");
      tree.add(apple, pear);
      tree.setSelectionMode(Tree.SelectionMode.SINGLE);
      List<String> events = new ArrayList<>();
      tree.onDeselect(event -> events.add("deselect " + event.getNode().getText()));
      tree.onSelect(event -> events.add("select " + event.getNode().getText()));
      frame.add(tree);
      tree.select(apple);

      UserInteraction.select(tree, pear);

      assertEquals(List.of("deselect Apple", "select Pear"), events);
      assertEquals(List.of(pear), tree.getSelectedItems());

      tree.setSelectionMode(Tree.SelectionMode.MULTIPLE);
      UserInteraction.select(tree, apple);

      assertEquals(List.of("deselect Apple", "select Pear", "select Apple"), events);
      assertEquals(List.of(pear, apple), tree.getSelectedItems());
    }
  }
}
