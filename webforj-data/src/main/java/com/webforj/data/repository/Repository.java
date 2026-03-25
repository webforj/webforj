package com.webforj.data.repository;

import com.webforj.data.HasEntityKey;
import com.webforj.data.concern.HasKeyProvider;
import com.webforj.data.repository.event.RepositoryCommitEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * A generic repository interface for data retrieval and counting.
 *
 * @param <T> The type of entities to be retrieved.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface Repository<T> extends HasKeyProvider<T> {

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
  Repository<T> setOffset(int offset);

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
  Repository<T> setLimit(int limit);

  /**
   * Gets the sorting criteria for the result.
   *
   * @return The order criteria list.
   */
  OrderCriteriaList<T> getOrderCriteriaList();

  /**
   * {@inheritDoc}
   *
   * <h3>Priority Order for Key Resolution:</h3>
   * <ol>
   * <li>Custom key provider (if set via this method)</li>
   * <li>{@link HasEntityKey} interface (if entity implements it)</li>
   * <li>Repository-specific default (e.g., JPA metadata, or entity itself)</li>
   * </ol>
   *
   * <h3>Usage Example:</h3>
   *
   * <pre>{@code
   * repository.setKeyProvider(Person::getId);
   * }</pre>
   */
  @Override
  Repository<T> setKeyProvider(Function<T, ?> keyProvider);

  /**
   * Gets the id/key of the provided entity.
   *
   * <p>
   * This method uses the key provider chain to determine the entity's key:
   * </p>
   * <ol>
   * <li>Custom key provider (if set)</li>
   * <li>{@link HasEntityKey} interface (if implemented)</li>
   * <li>Entity itself (backward compatible default)</li>
   * </ol>
   *
   * @param entity The entity for which to get the id.
   * @return The id of the entity.
   */
  default Object getKey(T entity) {
    Objects.requireNonNull(entity, "entity cannot be null");
    return getKeyProvider().apply(entity);
  }

  /**
   * Retrieves a stream of all entities.
   *
   * <p>
   * This method is used to retrieve all entities from the repository after applying any filters,
   * sorting, or pagination criteria that may have been set.
   * </p>
   *
   * @return A stream of all entities.
   */
  Stream<T> findAll();

  /**
   * Checks if the repository contains the provided entity.
   *
   * @param entity The entity to check.
   * @return true if the repository contains the entity, false otherwise.
   */
  default boolean has(T entity) {
    return find(getKey(entity)).isPresent();
  }

  /**
   * Retrieves an entity by its key.
   *
   * @param key The id of the entity to retrieve.
   * @return An Optional containing the found entity or empty if no entity found.
   */
  Optional<T> find(Object key);

  /**
   * Alias for {@link #find(Object)}.
   *
   * @param key The id of the entity to
   * @return An Optional containing the found entity or empty if no entity found
   */
  default Optional<T> findByKey(Object key) {
    return find(key);
  }

  /**
   * Counts the number of all entities.
   *
   * @return The count of all entities.
   */
  default int size() {
    return (int) findAll().count();
  }

  /**
   * Refreshes the given entity by reloading it from the data source.
   *
   * @param entity The entity to refresh.
   */
  Repository<T> commit(T entity);

  /**
   * Refreshes the repository by reloading all entities from the data source.
   */
  Repository<T> commit();

  /**
   * Adds a listener for the repository commit event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  ListenerRegistration<RepositoryCommitEvent<T>> addCommitListener(
      EventListener<RepositoryCommitEvent<T>> listener);

  /**
   * Alias for {@link #addCommitListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  default ListenerRegistration<RepositoryCommitEvent<T>> onCommit(
      EventListener<RepositoryCommitEvent<T>> listener) {
    return addCommitListener(listener);
  }
}
