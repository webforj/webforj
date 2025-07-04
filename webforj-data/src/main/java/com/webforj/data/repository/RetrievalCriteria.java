package com.webforj.data.repository;

import java.util.Comparator;
import java.util.function.Predicate;

/**
 * Represents a set of criteria for data retrieval, including filtering, sorting, limit, and offset.
 *
 * @param <T> The type of entities to which the criteria apply.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 * @deprecated This interface will be removed in version 26.00. Use {@link RepositoryCriteria} with
 *             the new {@link QueryableRepository} interface instead.
 */
@Deprecated(since = "25.02", forRemoval = true)
public interface RetrievalCriteria<T> {

  /**
   * Gets the starting position of records to retrieve.
   *
   * @return The starting position.
   */
  int getOffset();

  /**
   * Sets the starting position of records to retrieve and returns the Criteria instance.
   *
   * @param offset The starting position.
   * @return The Criteria instance.
   */
  RetrievalCriteria<T> setOffset(int offset);

  /**
   * Gets the maximum number of records to retrieve.
   *
   * @return The maximum number of records.
   */
  int getLimit();

  /**
   * Sets the maximum number of records to retrieve and returns the Criteria instance.
   *
   * @param limit The maximum number of records.
   * @return The Criteria instance.
   */
  RetrievalCriteria<T> setLimit(int limit);

  /**
   * Gets the criteria for filtering data.
   *
   * @return The filtering criteria.
   */
  Predicate<T> getFilter();

  /**
   * Sets the criteria for filtering data and returns the Criteria instance.
   *
   * @param filter The filtering criteria.
   * @return The Criteria instance.
   */
  RetrievalCriteria<T> setFilter(Predicate<T> filter);

  /**
   * Gets the sorting criteria for the result.
   *
   * @return The order criteria.
   */
  OrderCriteriaList<T> getOrderCriteriaList();

  /**
   * Gets the sorting criteria for the result.
   *
   * @return The order comparator.
   */
  Comparator<T> getOrderBy();

  /**
   * Sets the sorting criteria for the result and returns the Criteria instance.
   *
   * @param orderBy The sorting criteria.
   * @return The Criteria instance.
   */
  RetrievalCriteria<T> setOrderBy(Comparator<T> orderBy);
}
