package com.webforj.data.repository;

import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Repository that delegates data operations to user-provided functions.
 *
 * <p>
 * This repository implementation extends {@link AbstractQueryableRepository} and delegates all data
 * operations (fetch, count, find) to callback functions provided during construction. The callbacks
 * receive {@link RepositoryCriteria} objects containing filter, sort criteria, and pagination
 * parameters.
 * </p>
 *
 * <h3>Example: SQL Database</h3>
 *
 * <pre>{@code
 * DelegatingRepository<Product, ProductFilter> repo = new DelegatingRepository<>(
 *     criteria -> {
 *         String sql = "SELECT * FROM products";
 *         sql += buildWhere(criteria.getFilter());
 *         sql += buildOrderBy(criteria.getOrderCriteria());
 *         sql += " LIMIT " + criteria.getLimit() + " OFFSET " + criteria.getOffset();
 *         return jdbc.query(sql, rowMapper).stream();
 *     },
 *     criteria -> {
 *         String sql = "SELECT COUNT(*) FROM products" + buildWhere(criteria.getFilter());
 *         return jdbc.queryForObject(sql, Long.class);
 *     },
 *     key -> Optional.ofNullable(
 *         jdbc.queryForObject("SELECT * FROM products WHERE id = ?", rowMapper, key)
 *     )
 * );
 * }</pre>
 *
 * <h3>Example: Delegating to Another Repository</h3>
 *
 * <pre>{@code
 * CollectionRepository<Entity> backing = new CollectionRepository<>(data);
 * DelegatingRepository<Entity, Predicate<Entity>> repo = new DelegatingRepository<>(
 *     criteria -> backing.findBy(criteria),
 *     criteria -> backing.size(criteria),
 *     key -> backing.find(key)
 * );
 * }</pre>
 *
 * @param <T> Entity type
 * @param <F> Filter type (e.g., Predicate, Specification, custom filter object)
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class DelegatingRepository<T, F> extends AbstractQueryableRepository<T, F> {

  /**
   * Function to find entities based on criteria parameters.
   *
   * @param <T> Entity type
   * @param <F> Filter type
   */
  @FunctionalInterface
  public interface FindByCallback<T, F> {
    /**
     * Finds entities for the given criteria.
     *
     * @param criteria Contains filter, sort, and pagination
     * @return Stream of matching entities
     */
    Stream<T> findBy(RepositoryCriteria<T, F> criteria);
  }

  /**
   * Function to count entities based on criteria.
   *
   * @param <T> Entity type
   * @param <F> Filter type
   */
  @FunctionalInterface
  public interface SizeCallback<T, F> {
    /**
     * Counts entities matching the criteria.
     *
     * @param criteria Contains the filter
     * @return Count of matching entities
     */
    int size(RepositoryCriteria<T, F> criteria);
  }

  /**
   * Function to find entity by key.
   *
   * @param <T> Entity type
   */
  @FunctionalInterface
  public interface FindCallback<T> {
    /**
     * Finds entity by its key.
     *
     * @param key The key to search for
     * @return Entity if found
     */
    Optional<T> find(Object key);
  }

  private final FindByCallback<T, F> findByCallback;
  private final SizeCallback<T, F> countCallback;
  private final FindCallback<T> findCallback;

  /**
   * Creates a repository with custom operations.
   *
   * @param findByCallback Function to find entities based on criteria parameters
   * @param countCallback Function to count entities matching the criteria
   * @param findCallback Function to find entity by key
   * @throws NullPointerException if any callback is null
   */
  public DelegatingRepository(FindByCallback<T, F> findByCallback, SizeCallback<T, F> countCallback,
      FindCallback<T> findCallback) {
    this.findByCallback = Objects.requireNonNull(findByCallback, "FindBy callback cannot be null");
    this.countCallback = Objects.requireNonNull(countCallback, "Count callback cannot be null");
    this.findCallback = Objects.requireNonNull(findCallback, "Find callback cannot be null");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<T> find(Object key) {
    return findCallback.find(key);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Stream<T> findBy(RepositoryCriteria<T, F> criteria) {
    Objects.requireNonNull(criteria, "criteria cannot be null");
    return findByCallback.findBy(criteria);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int size(RepositoryCriteria<T, F> criteria) {
    Objects.requireNonNull(criteria, "criteria cannot be null");
    return countCallback.size(criteria);
  }
}
