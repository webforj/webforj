package com.webforj.data.repository;

import com.webforj.data.HasEntityKey;
import com.webforj.data.repository.event.RepositoryCommitEvent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

/**
 * An abstract implementation of the {@link Repository} interface.
 *
 * @param <T> The type of entities to be retrieved.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public abstract class AbstractRepository<T> implements Repository<T> {
  private final EventDispatcher eventDispatcher = new EventDispatcher();
  private int offset = 0;
  private int limit = Integer.MAX_VALUE;
  private final OrderCriteriaList<T> orderCriteriaList = new OrderCriteriaList<>();
  private Function<T, ?> keyProvider = null;

  /**
   * {@inheritDoc}
   */
  @Override
  public Repository<T> setKeyProvider(Function<T, ?> keyProvider) {
    Objects.requireNonNull(keyProvider, "Key provider cannot be null");
    this.keyProvider = keyProvider;
    // Trigger commit to notify listeners (e.g., Table) to update entity key mappings
    commit();
    return this;
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Returns the custom key provider if set, otherwise returns a default provider that checks for
   * {@link HasEntityKey} interface and falls back to returning the entity itself.
   * </p>
   */
  @Override
  public Function<T, ?> getKeyProvider() {
    if (keyProvider != null) {
      return keyProvider;
    }

    // check HasEntityKey, otherwise return entity
    return entity -> {
      if (entity instanceof HasEntityKey hasEntityKey) {
        return hasEntityKey.getEntityKey();
      }

      return entity;
    };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> setLimit(int limit) {
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
  public AbstractRepository<T> setOffset(int offset) {
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
  public OrderCriteriaList<T> getOrderCriteriaList() {
    return orderCriteriaList;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> commit() {
    fireCommitEvent(Collections.unmodifiableList(findAll().toList()), false);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> commit(T entity) {
    fireCommitEvent(List.of(entity), true);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  public ListenerRegistration<RepositoryCommitEvent<T>> addCommitListener(
      EventListener<RepositoryCommitEvent<T>> listener) {
    return getEventDispatcher().addListener(RepositoryCommitEvent.class, listener);
  }

  /**
   * Get the instance of the event dispatcher used by this repository.
   *
   * @return the event dispatcher
   */
  protected EventDispatcher getEventDispatcher() {
    return eventDispatcher;
  }

  /**
   * Fires a refresh event.
   *
   * @param commits The list of commits.
   * @param isSingle A flag to indicate if the event is for a single commit.
   */
  protected void fireCommitEvent(List<T> commits, boolean isSingle) {
    getEventDispatcher().dispatchEvent(new RepositoryCommitEvent<T>(this, commits, isSingle));
  }
}
