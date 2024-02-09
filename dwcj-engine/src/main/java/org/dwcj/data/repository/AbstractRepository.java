package org.dwcj.data.repository;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.dwcj.data.repository.event.RepositoryCommitEvent;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;

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
  private final RetrievalBuilder<T> defaultCriteria = new RetrievalBuilder<>();

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> setFilter(Predicate<T> filter) {
    defaultCriteria.setFilter(filter);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Predicate<T> getFilter() {
    return defaultCriteria.getFilter();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> setLimit(int limit) {
    defaultCriteria.setLimit(limit);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getLimit() {
    return defaultCriteria.getLimit();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> setOffset(int offset) {
    defaultCriteria.setOffset(offset);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getOffset() {
    return defaultCriteria.getOffset();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> setOrderBy(Comparator<T> orderBy) {
    defaultCriteria.setOrderBy(orderBy);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Comparator<T> getOrderBy() {
    return defaultCriteria.getOrderBy();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public OrderCriteriaList<T> getOrderCriteriaList() {
    return defaultCriteria.getOrderCriteriaList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Stream<T> findAll() {
    RetrievalBuilder<T> clone = new RetrievalBuilder<>();
    clone.setFilter(getFilter());
    clone.setLimit(getLimit());
    clone.setOffset(getOffset());
    clone.setOrderBy(getOrderBy());
    clone.getOrderCriteriaList().set(getOrderCriteriaList());

    if (clone.getOrderBy() == null) {
      clone.setOrderBy(new CompositeComparator<>(clone.getOrderCriteriaList()));
    }

    return findBy(clone);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> commit() {
    fireCommitEvent(Collections.unmodifiableList(findAll().toList()));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractRepository<T> commit(T entity) {
    fireCommitEvent(List.of(entity));
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
   */
  protected void fireCommitEvent(List<T> commits) {
    getEventDispatcher().dispatchEvent(new RepositoryCommitEvent<T>(this, commits));
  }
}
