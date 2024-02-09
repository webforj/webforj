package org.dwcj.data.repository;

import java.util.Comparator;
import java.util.function.Function;

/**
 * Represents a set of criteria for sorting data.
 *
 * @param <T> The type of entities to which the criteria apply.
 * @param <V> The type of the value to be sorted.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class OrderCriteria<T, V> {

  /**
   * The direction of the sort order.
   */
  public enum Direction {
    /**
     * Ascending (e.g. A-Z, 1..9) sort order.
     */
    ASC,
    /**
     * Descending (e.g. Z-A, 9..1) sort order.
     */
    DESC,
  }

  private final Function<T, V> valueProvider;
  private final Direction direction;
  private final Comparator<T> comparator;

  /**
   * Creates a new instance of OrderCriteria with the specified value provider and direction.
   *
   * @param valueProvider The function to provide the value to be sorted.
   * @param direction The direction of the sort order.
   * @param comparator The comparator to be used for sorting.
   */
  public OrderCriteria(Function<T, V> valueProvider, Direction direction,
      Comparator<T> comparator) {
    this.valueProvider = valueProvider;
    this.direction = direction;
    this.comparator = comparator;
  }

  /**
   * Creates a new instance of OrderCriteria with the specified value provider and direction.
   *
   * @param valueProvider The function to provide the value to be sorted.
   * @param direction The direction of the sort order.
   */
  public OrderCriteria(Function<T, V> valueProvider, Direction direction) {
    this(valueProvider, direction, null);
  }

  /**
   * Gets the value provider function.
   *
   * @return The value provider function.
   */
  public Function<T, V> getValueProvider() {
    return valueProvider;
  }

  /**
   * Gets the direction of the sort order.
   *
   * @return The direction of the sort order.
   */
  public Direction getDirection() {
    return direction;
  }

  /**
   * Gets the comparator to be used for sorting.
   *
   * @return The comparator to be used for sorting.
   */
  public Comparator<T> getComparator() {
    return comparator;
  }
}
