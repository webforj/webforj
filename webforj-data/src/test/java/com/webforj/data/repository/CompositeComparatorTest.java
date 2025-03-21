package com.webforj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import org.junit.jupiter.api.Test;

class CompositeComparatorTest {

  @Test
  void shouldCompareBasedOnSingleCriterion() {
    Function<String, Integer> valueProvider = String::length;
    OrderCriteria<String, Integer> orderCriteria =
        new OrderCriteria<>(valueProvider, OrderCriteria.Direction.ASC);
    OrderCriteriaList<String> criteria = new OrderCriteriaList<>(orderCriteria);
    CompositeComparator<String> comparator = new CompositeComparator<>(criteria);

    assertTrue(comparator.compare("abc", "abcd") < 0);
    assertTrue(comparator.compare("abcd", "abc") > 0);
    assertEquals(0, comparator.compare("abc", "abc"));
  }

  @Test
  void shouldCompareBasedOnLocalDate() {
    Function<LocalDate, Integer> yearProvider = LocalDate::getYear;
    OrderCriteria<LocalDate, Integer> orderCriteria =
        new OrderCriteria<>(yearProvider, OrderCriteria.Direction.ASC);
    CompositeComparator<LocalDate> comparator =
        new CompositeComparator<>(new OrderCriteriaList<>(orderCriteria));

    assertTrue(comparator.compare(LocalDate.of(2020, 1, 1), LocalDate.of(2021, 1, 1)) < 0);
    assertTrue(comparator.compare(LocalDate.of(2021, 1, 1), LocalDate.of(2020, 1, 1)) > 0);
    assertEquals(0, comparator.compare(LocalDate.of(2020, 1, 1), LocalDate.of(2020, 1, 1)));
  }

  @Test
  void shouldCompareBasedOnCustomClass() {
    Function<Person, String> nameProvider = Person::getName;
    Function<Person, LocalDate> birthDateProvider = Person::getBirthDate;

    OrderCriteria<Person, String> nameCriteria =
        new OrderCriteria<>(nameProvider, OrderCriteria.Direction.ASC);
    OrderCriteria<Person, LocalDate> birthDateCriteria =
        new OrderCriteria<>(birthDateProvider, OrderCriteria.Direction.DESC);

    List<OrderCriteria<Person, ?>> criteria = Arrays.asList(nameCriteria, birthDateCriteria);
    CompositeComparator<Person> comparator =
        new CompositeComparator<>(new OrderCriteriaList<>(criteria));

    Person john = new Person("John", LocalDate.of(1990, 1, 1));
    Person alice = new Person("Alice", LocalDate.of(1985, 1, 1));
    Person bob = new Person("Bob", LocalDate.of(1995, 1, 1));

    assertTrue(comparator.compare(john, alice) > 0);
    assertTrue(comparator.compare(alice, bob) < 0);
    assertTrue(comparator.compare(bob, john) < 0);
    assertEquals(0, comparator.compare(john, john));
  }

  @Test
  void shouldCompareBasedOnMultipleCriteria() {
    Function<String, Integer> lengthProvider = String::length;
    Function<String, String> valueProvider = s -> s;

    OrderCriteria<String, Integer> lengthCriteria =
        new OrderCriteria<>(lengthProvider, OrderCriteria.Direction.ASC);
    OrderCriteria<String, String> valueCriteria =
        new OrderCriteria<>(valueProvider, OrderCriteria.Direction.DESC);

    List<OrderCriteria<String, ?>> criteria = Arrays.asList(lengthCriteria, valueCriteria);
    CompositeComparator<String> comparator =
        new CompositeComparator<>(new OrderCriteriaList<>(criteria));

    assertTrue(comparator.compare("abc", "abcd") < 0);
    assertTrue(comparator.compare("abcd", "abc") > 0);
    assertTrue(comparator.compare("abc", "cba") > 0);
    assertEquals(0, comparator.compare("abc", "abc"));
  }

  @Test
  void shouldFallbackToStringComparisonWhenValuesAreNotComparable() {
    Function<NonComparableClass, NonComparableClass> valueProvider = Function.identity();
    OrderCriteria<NonComparableClass, NonComparableClass> orderCriteria =
        new OrderCriteria<>(valueProvider, OrderCriteria.Direction.ASC);

    CompositeComparator<NonComparableClass> comparator =
        new CompositeComparator<>(new OrderCriteriaList<>(orderCriteria), true);

    NonComparableClass obj1 = new NonComparableClass("abc");
    NonComparableClass obj2 = new NonComparableClass("abcd");

    assertTrue(comparator.compare(obj1, obj2) < 0);
    assertTrue(comparator.compare(obj2, obj1) > 0);
    assertEquals(0, comparator.compare(obj1, obj1));
  }

  @Test
  void shouldThrowExceptionWhenValuesAreNotComparableAndFallbackIsNotActivated() {
    Function<NonComparableClass, NonComparableClass> valueProvider = Function.identity();
    OrderCriteria<NonComparableClass, NonComparableClass> orderCriteria =
        new OrderCriteria<>(valueProvider, OrderCriteria.Direction.ASC);

    CompositeComparator<NonComparableClass> comparator =
        new CompositeComparator<>(new OrderCriteriaList<>(orderCriteria), false);

    NonComparableClass obj1 = new NonComparableClass("abc");
    NonComparableClass obj2 = new NonComparableClass("abcd");

    assertThrows(IllegalArgumentException.class, () -> comparator.compare(obj1, obj2));
  }

  @Test
  void shouldUseCustomComparatorForPersonObjects() {
    Function<Person, String> nameProvider = Person::getName;

    // Custom comparator for Person objects based on the length of their names
    Comparator<Person> customNameLengthComparator =
        Comparator.comparingInt(person -> person.getName().length());

    OrderCriteria<Person, String> customNameCriteria =
        new OrderCriteria<>(nameProvider, OrderCriteria.Direction.ASC, customNameLengthComparator);

    CompositeComparator<Person> comparator =
        new CompositeComparator<>(new OrderCriteriaList<>(customNameCriteria));

    Person person1 = new Person("Alice");
    Person person2 = new Person("Bob");
    Person person3 = new Person("Charlotte");

    // expect Bob < Alice < Charlotte
    assertTrue(comparator.compare(person1, person2) > 0);
    assertTrue(comparator.compare(person1, person3) < 0);
    assertTrue(comparator.compare(person2, person3) < 0);
  }

  @Test
  void shouldHandleNullValuesInComparison() {
    Function<String, Integer> lengthProvider = String::length;
    OrderCriteria<String, Integer> lengthCriteria =
        new OrderCriteria<>(lengthProvider, OrderCriteria.Direction.ASC);
    CompositeComparator<String> comparator =
        new CompositeComparator<>(new OrderCriteriaList<>(lengthCriteria));

    assertTrue(comparator.compare(null, "abc") < 0);
    assertTrue(comparator.compare("abc", null) < 0);
    assertEquals(0, comparator.compare(null, null));
  }

  @Test
  void shouldHandleNullValuesWithCustomComparator() {
    Function<Person, String> nameProvider = Person::getName;
    Comparator<Person> customNameLengthComparator =
        Comparator.comparingInt(person -> person.getName() != null ? person.getName().length() : 0);

    OrderCriteria<Person, String> customNameCriteria =
        new OrderCriteria<>(nameProvider, OrderCriteria.Direction.ASC, customNameLengthComparator);

    CompositeComparator<Person> comparator =
        new CompositeComparator<>(new OrderCriteriaList<>(customNameCriteria));

    Person person1 = new Person("Bob");
    Person person2 = new Person("Charlotte");

    assertTrue(comparator.compare(null, person1) < 0);
    assertTrue(comparator.compare(person2, null) < 0);
    assertEquals(0, comparator.compare(person1, person1));
  }

  class NonComparableClass {
    private String value;

    public NonComparableClass(String value) {
      this.value = value;
    }

    @Override
    public String toString() {
      return value;
    }
  }

  class Person {
    private String name;
    private LocalDate birthDate;

    public Person(String name, LocalDate birthDate) {
      this.name = name;
      this.birthDate = birthDate;
    }

    public Person(String name) {
      this.name = name;
    }

    public String getName() {
      return name;
    }

    public LocalDate getBirthDate() {
      return birthDate;
    }
  }
}
