package com.webforj.data.repository;

import java.util.Objects;

/**
 * Query parameters for repository operations.
 *
 * <p>
 * Immutable object containing filter, sort criteria, offset, and limit.
 * </p>
 *
 * @param <T> entity type
 * @param <F> filter type
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class RepositoryCriteria<T, F> {
  private final int offset;
  private final int limit;
  private final OrderCriteriaList<T> orderCriteria;
  private final F filter;

  /**
   * Creates a query with all parameters.
   *
   * @param offset items to skip (>= 0)
   * @param limit max items to return (>= 0)
   * @param orderCriteria sort criteria (null for unsorted)
   * @param filter filter to apply (null for no filter)
   *
   * @throws IllegalArgumentException if offset or limit is negative
   */
  public RepositoryCriteria(int offset, int limit, OrderCriteriaList<T> orderCriteria, F filter) {
    if (offset < 0) {
      throw new IllegalArgumentException("Offset must be non-negative, but was: " + offset);
    }

    if (limit < 0) {
      throw new IllegalArgumentException("Limit must be non-negative, but was: " + limit);
    }

    this.offset = offset;
    this.limit = limit;
    this.orderCriteria = orderCriteria != null ? orderCriteria : new OrderCriteriaList<>();
    this.filter = filter;
  }

  /**
   * Creates a query with filter only (no pagination or sorting).
   *
   * @param filter filter to apply
   */
  public RepositoryCriteria(F filter) {
    this(0, Integer.MAX_VALUE, null, filter);
  }

  /**
   * Creates a query with pagination only (no filter or sorting).
   *
   * @param offset items to skip (>= 0)
   * @param limit max items to return (>= 0)
   *
   * @throws IllegalArgumentException if offset or limit is negative
   */
  public RepositoryCriteria(int offset, int limit) {
    this(offset, limit, null, null);
  }

  /**
   * Creates a query with filter and pagination (no sorting).
   *
   * @param offset items to skip (>= 0)
   * @param limit max items to return (>= 0)
   * @param filter filter to apply
   *
   * @throws IllegalArgumentException if offset or limit is negative
   */
  public RepositoryCriteria(int offset, int limit, F filter) {
    this(offset, limit, null, filter);
  }

  /**
   * Gets the offset.
   *
   * @return number of items to skip
   */
  public int getOffset() {
    return offset;
  }

  /**
   * Gets the limit.
   *
   * @return max items to return
   */
  public int getLimit() {
    return limit;
  }

  /**
   * Gets the sort criteria.
   *
   * @return order criteria list (never null, may be empty)
   */
  public OrderCriteriaList<T> getOrderCriteria() {
    return orderCriteria;
  }

  /**
   * Gets the filter.
   *
   * @return filter to apply, or null
   */
  public F getFilter() {
    return filter;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }

    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    RepositoryCriteria<?, ?> that = (RepositoryCriteria<?, ?>) o;
    return offset == that.offset && limit == that.limit
        && Objects.equals(orderCriteria, that.orderCriteria) && Objects.equals(filter, that.filter);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode() {
    return Objects.hash(offset, limit, orderCriteria, filter);
  }
}
