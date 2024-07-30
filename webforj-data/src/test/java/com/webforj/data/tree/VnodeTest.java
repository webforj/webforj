package com.webforj.data.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import org.junit.jupiter.api.Test;

class VnodeTest {

  @Test
  void shouldGetData() {
    Vnode<String> route = new Vnode<>("root");
    assertEquals("root", route.getData());
  }

  @Test
  void shouldSetAndGetParent() {
    Vnode<String> parent = new Vnode<>("parent");
    Vnode<String> child = new Vnode<>("child");

    child.setParent(parent);
    assertEquals(parent, child.getParent());
  }

  @Test
  void shouldAddChild() {
    Vnode<String> parent = new Vnode<>("parent");
    Vnode<String> child = new Vnode<>("child");

    parent.addChild(child);
    List<Vnode<String>> children = parent.getChildren();
    assertEquals(1, children.size());
    assertTrue(children.contains(child));
    assertEquals(parent, child.getParent());
  }

  @Test
  void shouldRemoveChild() {
    Vnode<String> parent = new Vnode<>("parent");
    Vnode<String> child = new Vnode<>("child");

    parent.addChild(child);
    parent.removeChild(child);
    List<Vnode<String>> children = parent.getChildren();
    assertEquals(0, children.size());
    assertNull(child.getParent());
  }

  @Test
  void shouldRemoveAllChildren() {
    Vnode<String> parent = new Vnode<>("parent");
    Vnode<String> child1 = new Vnode<>("child1");
    Vnode<String> child2 = new Vnode<>("child2");

    parent.addChild(child1);
    parent.addChild(child2);
    parent.removeAllChildren();
    List<Vnode<String>> children = parent.getChildren();
    assertEquals(0, children.size());
    assertNull(child1.getParent());
    assertNull(child2.getParent());
  }

  @Test
  void shouldIterateOverTree() {
    Vnode<String> root = new Vnode<>("root");
    Vnode<String> child1 = new Vnode<>("child1");
    Vnode<String> child2 = new Vnode<>("child2");
    Vnode<String> grandchild = new Vnode<>("grandchild");

    root.addChild(child1);
    root.addChild(child2);
    child1.addChild(grandchild);

    Iterator<Vnode<String>> iterator = root.iterator();
    assertTrue(iterator.hasNext());
    assertEquals("root", iterator.next().getData());
    assertTrue(iterator.hasNext());
    assertEquals("child1", iterator.next().getData());
    assertTrue(iterator.hasNext());
    assertEquals("grandchild", iterator.next().getData());
    assertTrue(iterator.hasNext());
    assertEquals("child2", iterator.next().getData());
    assertFalse(iterator.hasNext());
  }

  @Test
  void shouldThrowExceptionWhenNoMoreElements() {
    Vnode<String> root = new Vnode<>("root");
    Iterator<Vnode<String>> iterator = root.iterator();

    assertTrue(iterator.hasNext());
    assertEquals("root", iterator.next().getData());
    assertFalse(iterator.hasNext());

    assertThrows(NoSuchElementException.class, iterator::next);
  }
}
