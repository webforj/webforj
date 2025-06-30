package com.webforj.data.repository;

import com.webforj.data.repository.event.RepositoryCommitEvent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

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
  private Comparator<T> orderBy = null;
  private Predicate<T> filter = null;
  private final OrderCriteriaList<T> orderCriteriaList = new OrderCriteriaList<>();

  /**
   * {@inheritDoc}
   *
   * @deprecated since 25.02, use {@link #setBaseFilter(Predicate)} instead.
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public AbstractRepository<T> setFilter(Predicate<T> filter) {
    this.filter = filter;
    return this;
  }

  /**
   * {@inheritDoc}
   *
   * @deprecated since 25.02, use {@link #getBaseFilter()} instead.
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public Predicate<T> getFilter() {
    return filter;
  }

  /**
   * {@inheritDoc}
   *
   * @deprecated since 25.02.
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public AbstractRepository<T> setOrderBy(Comparator<T> orderBy) {
    this.orderBy = orderBy;
    return this;
  }

  /**
   * {@inheritDoc}
   *
   * @deprecated since 25.02, use {@link #getOrderCriteriaList()} instead.
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public Comparator<T> getOrderBy() {
    return orderBy;
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
