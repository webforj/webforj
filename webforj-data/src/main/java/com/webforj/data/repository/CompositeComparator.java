package com.webforj.data.repository;

import java.util.Comparator;

/**
 * A comparator that compares objects based on a set of criteria using a list of criteria.
 *
 * @param <T> The type of objects to be compared.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class CompositeComparator<T> implements Comparator<T> {

  private final OrderCriteriaList<T> criteria;
  private final boolean fallbackToStringComparison;

  /**
   * Creates a new instance of CompositeComparator with the specified criteria list.
   *
   * @param criteriaList The criteria list to be used for comparison.
   * @param fallbackToStringComparison Whether to fallback to string comparison if the values
   *        provided are not Comparable.
   */
  public CompositeComparator(OrderCriteriaList<T> criteriaList,
      boolean fallbackToStringComparison) {
    this.criteria = criteriaList;
    this.fallbackToStringComparison = fallbackToStringComparison;
  }

  /**
   * Creates a new instance of CompositeComparator with the specified criteria list.
   *
   * @param criteriaList The criteria list to be used for comparison.
   */
  public CompositeComparator(OrderCriteriaList<T> criteriaList) {
    this(criteriaList, false);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int compare(T o1, T o2) {
    for (OrderCriteria<T, ?> criterion : criteria) {
      Comparator<T> comparator = createComparator(criterion);
      int result = comparator.compare(o1, o2);
      if (result != 0) {
        return result;
      }
    }

    return 0;
  }

  /**
   * Creates a comparator for the specified criterion.
   *
   * @param criterion The criterion for which the comparator is to be created.
   * @return A comparator for the specified criterion.
   */
  @SuppressWarnings({"rawtypes", "unchecked"})
  protected Comparator<T> createComparator(OrderCriteria<T, ?> criterion) {
    return (o1, o2) -> {
      try {
        if (criterion.getComparator() != null) {
          // Use the custom comparator if provided
          Comparator<T> customComparator = criterion.getComparator();

          if (criterion.getDirection() == OrderCriteria.Direction.DESC) {
            customComparator = customComparator.reversed();
          }

          return customComparator.compare(o1, o2);
        } else {
          // Attempt to cast and compare as Comparable
          Comparable value1 = (Comparable) criterion.getValueProvider().apply(o1);
          Comparable value2 = (Comparable) criterion.getValueProvider().apply(o2);

          return compareWithDirection(value1, value2, criterion.getDirection());
        }
      } catch (ClassCastException e) {
        if (fallbackToStringComparison) {
          // Fallback to string comparison if the values are not Comparable
          String value1Str = String.valueOf(criterion.getValueProvider().apply(o1));
          String value2Str = String.valueOf(criterion.getValueProvider().apply(o2));

          return compareWithDirection(value1Str, value2Str, criterion.getDirection());
        } else {
          throw new IllegalArgumentException("Values are not comparable", e);
        }
      }
    };
  }

  /**
   * Compares two values with the specified direction.
   *
   * @param value1 The first value to be compared.
   * @param value2 The second value to be compared.
   * @param direction The direction of the comparison.
   * @param <V> The type of the values to be compared.
   *
   * @return A negative integer, zero, or a positive integer as the first argument is less than,
   *         equal to, or greater than the second.
   */
  protected <V extends Comparable<V>> int compareWithDirection(V value1, V value2,
      OrderCriteria.Direction direction) {
    int comparison = value1.compareTo(value2);
    return direction == OrderCriteria.Direction.ASC ? comparison : -comparison;
  }
}
