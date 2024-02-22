package org.dwcj.data.repository;

import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;
import org.dwcj.data.HasEntityKey;
import org.dwcj.data.repository.event.RepositoryCommitEvent;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;

/**
 * A generic repository interface for data retrieval and counting.
 *
 * @param <T> The type of entities to be retrieved.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface Repository<T> extends RetrievalCriteria<T> {

  /**
   * Gets the id/key of the provided entity.
   *
   * @param entity The entity for which to get the id.
   * @return The id of the entity.
   */
  default Object getKey(T entity) {
    Objects.requireNonNull(entity, "entity cannot be null");

    if (entity instanceof HasEntityKey) {
      return ((HasEntityKey) entity).getEntityKey();
    } else {
      return entity;
    }
  }

  /**
   * Retrieves the index of the provided entity.
   *
   * @param entity The entity for which to get the index.
   * @return The index of the entity or -1 if the entity is not found.
   */
  int getIndex(T entity);

  /**
   * Clears the repository of all entities.
   *
   * @return The repository instance.
   */
  Repository<T> clear();

  /**
   * Retrieves a stream of all entities.
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
   * Retrieves an entity by its index.
   *
   * @param index The index of the entity to retrieve.
   * @return An Optional containing the found entity or empty if no entity found.
   */
  Optional<T> findByIndex(int index);

  /**
   * Retrieves a stream of entities based on the provided criteria.
   *
   * @param criteria The criteria to use to find the entities.
   * @return A stream of entities that match the criteria.
   */
  Stream<T> findBy(RetrievalCriteria<T> criteria);

  /**
   * Finds a single entity based on the provided criteria.
   *
   * @param criteria The criteria to use to find the entity.
   * @return An Optional containing the found entity or empty if no entity found.
   */
  default Optional<T> findOneBy(RetrievalCriteria<T> criteria) {
    return findBy(criteria).findFirst();
  }

  /**
   * Counts the number of entities that match the provided criteria.
   *
   * @param criteria The criteria for counting data.
   * @return The count of entities that match the criteria.
   */
  default int size(RetrievalCriteria<T> criteria) {
    return (int) findBy(criteria).count();
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
