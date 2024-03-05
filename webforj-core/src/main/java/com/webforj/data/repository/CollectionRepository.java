package com.webforj.data.repository;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * A simple implementation of the {@link Repository} interface for list data retrieval.
 *
 * @param <T> The type of entities to be retrieved.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class CollectionRepository<T> extends AbstractRepository<T> {
  private final Collection<T> items;

  /**
   * Creates a new instance of ListRepository with the provided list of items.
   *
   * @param items The list of items to be used for data retrieval.
   */
  public CollectionRepository(Collection<T> items) {
    this.items = items;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getIndex(T entity) {
    return new ArrayList<>(items).indexOf(entity);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CollectionRepository<T> clear() {
    items.clear();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<T> find(Object id) {
    return items.stream().filter(item -> getKey(item).equals(id)).findFirst();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<T> findByIndex(int index) {
    List<T> list = new ArrayList<>(items);

    if (index >= 0 && index < list.size()) {
      return Optional.ofNullable(list.get(index));
    } else {
      return Optional.empty();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Stream<T> findBy(RetrievalCriteria<T> criteria) {
    if (criteria == null) {
      return items.stream();
    }

    Predicate<T> filter = criteria.getFilter();
    Comparator<T> orderBy = criteria.getOrderBy();
    int limit = criteria.getLimit();
    int offset = criteria.getOffset();

    Stream<T> stream = items.stream().filter(entity -> filter == null || filter.test(entity));

    if (orderBy != null) {
      stream = stream.sorted(orderBy);
    }

    if (offset > 0) {
      stream = stream.skip(offset);
    }

    if (limit > 0) {
      stream = stream.limit(limit);
    }

    return stream;
  }
}
