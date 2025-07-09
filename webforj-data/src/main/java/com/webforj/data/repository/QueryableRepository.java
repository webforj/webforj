package com.webforj.data.repository;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Repository that executes queries with custom filter types.
 *
 * @param <T> entity type
 * @param <F> filter type (Predicate, Specification, Query, etc.)
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public interface QueryableRepository<T, F> extends Repository<T> {

  /**
   * Sets the filter. Replaces any existing filter.
   *
   * @param filter the filter to apply, or {@code null} to clear
   * @return this repository instance for method chaining
   *
   * @see #setFilter(Object)
   */
  QueryableRepository<T, F> setBaseFilter(F filter);

  /**
   * Alias for {@link #setBaseFilter(Object)}.
   *
   * @param filter the filter to apply, or {@code null} to clear
   */
  default QueryableRepository<T, F> setFilter(F filter) {
    return setBaseFilter(filter);
  }

  /**
   * Gets the current filter.
   *
   * @return the filter, or {@code null} if none
   */
  F getBaseFilter();

  /**
   * Executes the query and returns matching entities.
   *
   * @param query contains filter, sort criteria, offset, and limit
   * @return stream of matching entities
   * @throws NullPointerException if query is null
   */
  Stream<T> findBy(RepositoryCriteria<T, F> query);

  /**
   * Finds a single entity matching the query.
   *
   * <p>
   * If multiple entities match, only the first one is returned.
   * </p>
   *
   * @param query contains filter, sort criteria, offset, and limit
   * @return stream containing a single entity or empty if not found
   * @throws NullPointerException if query is null
   */
  default Optional<T> findOneBy(RepositoryCriteria<T, F> query) {
    return findBy(query).findFirst();
  }

  /**
   * Counts entities matching the filter. Ignores sorting and pagination.
   *
   * @param query only the filter is used
   * @return count of matching entities
   * @throws NullPointerException if query is null
   */
  int size(RepositoryCriteria<T, F> query);

  /**
   * Alias for {@link #size(RepositoryCriteria)}.
   *
   * @param query only the filter is used
   * @return count of matching entities
   * @throws NullPointerException if query is null
   */
  default int sizeBy(RepositoryCriteria<T, F> query) {
    return size(query);
  }
}
