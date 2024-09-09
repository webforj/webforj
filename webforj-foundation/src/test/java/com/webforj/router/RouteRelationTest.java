package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import org.junit.jupiter.api.Test;

class RouteRelationTest {

  @Test
  void shouldGetData() {
    RouteRelation<String> route = new RouteRelation<>("root");
    assertEquals("root", route.getData());
  }

  @Test
  void shouldSetAndGetParent() {
    RouteRelation<String> parent = new RouteRelation<>("parent");
    RouteRelation<String> child = new RouteRelation<>("child");

    child.setParent(parent);
    assertEquals(parent, child.getParent());
  }

  @Test
  void shouldAddChild() {
    RouteRelation<String> parent = new RouteRelation<>("parent");
    RouteRelation<String> child = new RouteRelation<>("child");

    parent.addChild(child);
    List<RouteRelation<String>> children = parent.getChildren();
    assertEquals(1, children.size());
    assertTrue(children.contains(child));
    assertEquals(parent, child.getParent());
  }

  @Test
  void shouldRemoveChild() {
    RouteRelation<String> parent = new RouteRelation<>("parent");
    RouteRelation<String> child = new RouteRelation<>("child");

    parent.addChild(child);
    parent.removeChild(child);
    List<RouteRelation<String>> children = parent.getChildren();
    assertEquals(0, children.size());
    assertNull(child.getParent());
  }

  @Test
  void shouldRemoveAllChildren() {
    RouteRelation<String> parent = new RouteRelation<>("parent");
    RouteRelation<String> child1 = new RouteRelation<>("child1");
    RouteRelation<String> child2 = new RouteRelation<>("child2");

    parent.addChild(child1);
    parent.addChild(child2);
    parent.removeAllChildren();
    List<RouteRelation<String>> children = parent.getChildren();
    assertEquals(0, children.size());
    assertNull(child1.getParent());
    assertNull(child2.getParent());
  }

  @Test
  void shouldIterateOverTree() {
    RouteRelation<String> root = new RouteRelation<>("root");
    RouteRelation<String> child1 = new RouteRelation<>("child1");
    RouteRelation<String> child2 = new RouteRelation<>("child2");
    RouteRelation<String> grandchild = new RouteRelation<>("grandchild");

    root.addChild(child1);
    root.addChild(child2);
    child1.addChild(grandchild);

    Iterator<RouteRelation<String>> iterator = root.iterator();
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
    RouteRelation<String> root = new RouteRelation<>("root");
    Iterator<RouteRelation<String>> iterator = root.iterator();

    assertTrue(iterator.hasNext());
    assertEquals("root", iterator.next().getData());
    assertFalse(iterator.hasNext());

    assertThrows(NoSuchElementException.class, iterator::next);
  }
}
