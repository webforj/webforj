package com.webforj.data.repository;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * A simple in-memory repository implementation of the {@link Repository} interface for list data
 * retrieval.
 *
 * @param <T> The type of entities to be retrieved.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class CollectionRepository<T> extends AbstractRepository<T>
    implements QueryableRepository<T, Predicate<T>> {
  private final Collection<T> items;
  private Predicate<T> baseFilter = null;

  /**
   * Creates a new instance of CollectionRepository with the provided list of items.
   *
   * @param items The list of items to be used for data retrieval.
   */
  public CollectionRepository(Collection<T> items) {
    this.items = items;
    setKeyProvider(entity -> {
      if (entity instanceof com.webforj.data.HasEntityKey hasEntityKey) {
        return hasEntityKey.getEntityKey();
      }

      return entity;
    });
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CollectionRepository<T> setBaseFilter(Predicate<T> filter) {
    this.baseFilter = filter;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Predicate<T> getBaseFilter() {
    return baseFilter;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Stream<T> findAll() {
    RepositoryCriteria<T, Predicate<T>> query =
        new RepositoryCriteria<>(getOffset(), getLimit(), getOrderCriteriaList(), getBaseFilter());
    return findBy(query);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Stream<T> findBy(RepositoryCriteria<T, Predicate<T>> query) {
    Stream<T> stream = items.stream();

    // Apply filter if present
    if (query.getFilter() != null) {
      stream = stream.filter(query.getFilter());
    }

    // Apply sorting
    OrderCriteriaList<T> orderCriteria = query.getOrderCriteria();
    if (orderCriteria != null && orderCriteria.size() > 0) {
      // Use CompositeComparator to handle sorting
      Comparator<T> comparator = new CompositeComparator<>(orderCriteria);
      stream = stream.sorted(comparator);
    }

    // Apply pagination
    if (query.getOffset() > 0) {
      stream = stream.skip(query.getOffset());
    }

    if (query.getLimit() > 0) {
      stream = stream.limit(query.getLimit());
    }

    return stream;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<T> find(Object key) {
    return items.stream().filter(item -> getKey(item).equals(key)).findFirst();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int size(RepositoryCriteria<T, Predicate<T>> query) {
    Stream<T> stream = items.stream();

    // Apply filter if present
    if (query.getFilter() != null) {
      stream = stream.filter(query.getFilter());
    }

    return (int) stream.count();
  }
}
