package com.webforj.devtools.craftforj.utilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("ComponentTypeNames")
class ComponentTypeNamesTest {

  static class Grandparent {
  }

  static class Parent extends Grandparent {
  }

  static class Child extends Parent {
  }

  @Nested
  @DisplayName("of(Class)")
  class Of {

    @Test
    @DisplayName("collects the simple names of the class and all its superclasses")
    void shouldCollectHierarchySimpleNames() {
      Set<String> names = ComponentTypeNames.of(Child.class);

      assertEquals(Set.of("Child", "Parent", "Grandparent"), names);
    }

    @Test
    @DisplayName("orders names from most specific to least specific")
    void shouldOrderMostSpecificFirst() {
      Set<String> names = ComponentTypeNames.of(Child.class);

      assertEquals(List.of("Child", "Parent", "Grandparent"), List.copyOf(names));
    }

    @Test
    @DisplayName("stops before Object and never includes it")
    void shouldStopBeforeObject() {
      Set<String> names = ComponentTypeNames.of(Grandparent.class);

      assertEquals(Set.of("Grandparent"), names);
      assertFalse(names.contains("Object"));
    }

    @Test
    @DisplayName("skips the empty simple name of an anonymous class but keeps its superclasses")
    void shouldSkipAnonymousClassSimpleName() {
      Parent anonymous = new Parent() {};

      Set<String> names = ComponentTypeNames.of(anonymous.getClass());

      assertEquals(Set.of("Parent", "Grandparent"), names);
      assertTrue(names.stream().noneMatch(String::isEmpty));
    }
  }
}
