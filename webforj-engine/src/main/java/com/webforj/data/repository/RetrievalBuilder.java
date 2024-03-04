package com.webforj.data.repository;

import java.util.Comparator;
import java.util.function.Predicate;

/**
 * Represents a build of a repository criteria for data retrieval, including filtering, sorting,
 * limit, and offset.
 *
 * @param <T> The type of entities to which the criteria apply.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class RetrievalBuilder<T> implements RetrievalCriteria<T> {
  private int offset;
  private int limit;
  private Comparator<T> orderBy;
  private Predicate<T> filter;
  private OrderCriteriaList<T> orderCriteriaList = new OrderCriteriaList<>();

  /**
   * Creates a new instance of Criteria with default parameters.
   */
  public RetrievalBuilder() {
    this(0, Integer.MAX_VALUE);
  }

  /**
   * Creates a new instance of Criteria with the specified offset and limit.
   *
   * @param offset The starting position of records to retrieve.
   * @param limit The maximum number of records to retrieve.
   */
  public RetrievalBuilder(int offset, int limit) {
    this(offset, limit, null, t -> true);
  }

  /**
   * Creates a new instance of Criteria with the specified filtering criteria.
   *
   * @param filter The criteria for filtering data.
   */
  public RetrievalBuilder(Predicate<T> filter) {
    this(0, Integer.MAX_VALUE, null, filter);
  }

  /**
   * Creates a new instance of Criteria with the specified sorting criteria.
   *
   * @param orderBy The criteria for sorting the result.
   */
  public RetrievalBuilder(Comparator<T> orderBy) {
    this(0, Integer.MAX_VALUE, orderBy, t -> true);
  }

  /**
   * Creates a new instance of Criteria with the specified parameters.
   *
   * @param offset The starting position of records to retrieve.
   * @param limit The maximum number of records to retrieve.
   * @param orderBy The criteria for sorting the result.
   * @param filter The criteria for filtering data.
   */
  public RetrievalBuilder(int offset, int limit, Comparator<T> orderBy, Predicate<T> filter) {
    this.offset = offset;
    this.limit = limit;
    this.orderBy = orderBy;
    this.filter = filter;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RetrievalCriteria<T> setOffset(int offset) {
    this.offset = offset;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getOffset() {
    return offset;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RetrievalCriteria<T> setLimit(int limit) {
    this.limit = limit;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getLimit() {
    return limit;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RetrievalCriteria<T> setFilter(Predicate<T> filter) {
    this.filter = filter;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Predicate<T> getFilter() {
    return filter;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RetrievalCriteria<T> setOrderBy(Comparator<T> orderBy) {
    this.orderBy = orderBy;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Comparator<T> getOrderBy() {
    return orderBy;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public OrderCriteriaList<T> getOrderCriteriaList() {
    return orderCriteriaList;
  }
}
