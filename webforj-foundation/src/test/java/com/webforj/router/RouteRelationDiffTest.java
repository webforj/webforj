package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class RouteRelationDiffTest {

  private RouteRelation<String> root1;
  private RouteRelation<String> root2;

  @BeforeEach
  void setUp() {
    root1 = new RouteRelation<>("root");
    RouteRelation<String> child1 = new RouteRelation<>("child1");
    RouteRelation<String> child2 = new RouteRelation<>("child2");
    root1.addChild(child1);
    root1.addChild(child2);

    root2 = new RouteRelation<>("root");
    RouteRelation<String> child3 = new RouteRelation<>("child1");
    RouteRelation<String> child4 = new RouteRelation<>("child3");
    root2.addChild(child3);
    root2.addChild(child4);
  }

  @Test
  void shouldAddAllWhenCurrentIsNull() {
    RouteRelationDiff<String> diff = new RouteRelationDiff<>(null, root1);

    assertEquals(Set.of("root", "child1", "child2"), diff.getToAdd());
    assertEquals(Set.of(), diff.getToRemove());
    assertEquals(Set.of(), diff.getToKeep());
  }

  @Test
  void shouldRemoveAllWhenNextIsNull() {
    RouteRelationDiff<String> diff = new RouteRelationDiff<>(root1, null);

    assertEquals(Set.of(), diff.getToAdd());
    assertEquals(Set.of("root", "child1", "child2"), diff.getToRemove());
    assertEquals(Set.of(), diff.getToKeep());
  }

  @Test
  void shouldKeepRootAndAddAndRemoveChildren() {
    RouteRelationDiff<String> diff = new RouteRelationDiff<>(root1, root2);

    assertEquals(Set.of("child3"), diff.getToAdd());
    assertEquals(Set.of("child2"), diff.getToRemove());
    assertEquals(Set.of("root", "child1"), diff.getToKeep());
  }

  @Test
  void shouldHandleIdenticalTrees() {
    RouteRelationDiff<String> diff = new RouteRelationDiff<>(root1, root1);

    assertEquals(Set.of(), diff.getToAdd());
    assertEquals(Set.of(), diff.getToRemove());
    assertEquals(Set.of("root", "child1", "child2"), diff.getToKeep());
  }

  @Test
  void shouldHandleDifferentRoots() {
    RouteRelation<String> newRoot = new RouteRelation<>("newRoot");
    RouteRelationDiff<String> diff = new RouteRelationDiff<>(root1, newRoot);

    assertEquals(Set.of("newRoot"), diff.getToAdd());
    assertEquals(Set.of("root", "child1", "child2"), diff.getToRemove());
    assertEquals(Set.of(), diff.getToKeep());
  }

  @Test
  void shouldHandleComplexTrees() {
    RouteRelation<String> child1 = new RouteRelation<>("child1");
    RouteRelation<String> child2 = new RouteRelation<>("child2");
    RouteRelation<String> child3 = new RouteRelation<>("child3");

    RouteRelation<String> newChild1 = new RouteRelation<>("child1");
    RouteRelation<String> newChild2 = new RouteRelation<>("newChild2");
    RouteRelation<String> newChild3 = new RouteRelation<>("child3");

    root1.addChild(child1);
    child1.addChild(child2);
    child2.addChild(child3);

    root2.addChild(newChild1);
    newChild1.addChild(newChild2);
    newChild2.addChild(newChild3);

    RouteRelationDiff<String> diff = new RouteRelationDiff<>(root1, root2);

    assertEquals(Set.of("newChild2", "child3"), diff.getToAdd());
    assertEquals(Set.of("child2"), diff.getToRemove());
    assertEquals(Set.of("root", "child1"), diff.getToKeep());
  }
}
