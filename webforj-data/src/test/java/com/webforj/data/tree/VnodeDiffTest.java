package com.webforj.data.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class VnodeDiffTest {

  private Vnode<String> root1;
  private Vnode<String> root2;

  @BeforeEach
  void setUp() {
    root1 = new Vnode<>("root");
    Vnode<String> child1 = new Vnode<>("child1");
    Vnode<String> child2 = new Vnode<>("child2");
    root1.addChild(child1);
    root1.addChild(child2);

    root2 = new Vnode<>("root");
    Vnode<String> child3 = new Vnode<>("child1");
    Vnode<String> child4 = new Vnode<>("child3");
    root2.addChild(child3);
    root2.addChild(child4);
  }

  @Test
  void shouldAddAllWhenCurrentIsNull() {
    VnodeDiff<String> diff = new VnodeDiff<>(null, root1);

    assertEquals(Set.of("root", "child1", "child2"), diff.getToAdd());
    assertEquals(Set.of(), diff.getToRemove());
    assertEquals(Set.of(), diff.getToKeep());
  }

  @Test
  void shouldRemoveAllWhenNextIsNull() {
    VnodeDiff<String> diff = new VnodeDiff<>(root1, null);

    assertEquals(Set.of(), diff.getToAdd());
    assertEquals(Set.of("root", "child1", "child2"), diff.getToRemove());
    assertEquals(Set.of(), diff.getToKeep());
  }

  @Test
  void shouldKeepRootAndAddAndRemoveChildren() {
    VnodeDiff<String> diff = new VnodeDiff<>(root1, root2);

    assertEquals(Set.of("child3"), diff.getToAdd());
    assertEquals(Set.of("child2"), diff.getToRemove());
    assertEquals(Set.of("root", "child1"), diff.getToKeep());
  }

  @Test
  void shouldHandleIdenticalTrees() {
    VnodeDiff<String> diff = new VnodeDiff<>(root1, root1);

    assertEquals(Set.of(), diff.getToAdd());
    assertEquals(Set.of(), diff.getToRemove());
    assertEquals(Set.of("root", "child1", "child2"), diff.getToKeep());
  }

  @Test
  void shouldHandleDifferentRoots() {
    Vnode<String> newRoot = new Vnode<>("newRoot");
    VnodeDiff<String> diff = new VnodeDiff<>(root1, newRoot);

    assertEquals(Set.of("newRoot"), diff.getToAdd());
    assertEquals(Set.of("root", "child1", "child2"), diff.getToRemove());
    assertEquals(Set.of(), diff.getToKeep());
  }

  @Test
  void shouldHandleComplexTrees() {
    Vnode<String> child1 = new Vnode<>("child1");
    Vnode<String> child2 = new Vnode<>("child2");
    Vnode<String> child3 = new Vnode<>("child3");

    Vnode<String> newChild1 = new Vnode<>("child1");
    Vnode<String> newChild2 = new Vnode<>("newChild2");
    Vnode<String> newChild3 = new Vnode<>("child3");

    root1.addChild(child1);
    child1.addChild(child2);
    child2.addChild(child3);

    root2.addChild(newChild1);
    newChild1.addChild(newChild2);
    newChild2.addChild(newChild3);

    VnodeDiff<String> diff = new VnodeDiff<>(root1, root2);

    assertEquals(Set.of("newChild2", "child3"), diff.getToAdd());
    assertEquals(Set.of("child2"), diff.getToRemove());
    assertEquals(Set.of("root", "child1"), diff.getToKeep());
  }
}
