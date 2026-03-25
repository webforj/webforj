package com.webforj.data.repository;

import java.util.stream.Stream;

/**
 * Abstract base class for repositories with queryable capabilities.
 *
 * <p>
 * This class provides a foundation for implementing repositories that support custom filter types
 * and query-based operations. It extends {@link AbstractRepository} and implements
 * {@link QueryableRepository}.
 * </p>
 *
 * @param <T> The entity type
 * @param <F> The filter type (e.g., Predicate, Specification, custom filter object)
 *
 * @see QueryableRepository
 * @see RepositoryCriteria
 * @see DelegatingRepository
 * @see CollectionRepository
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public abstract class AbstractQueryableRepository<T, F> extends AbstractRepository<T>
    implements QueryableRepository<T, F> {

  private F baseFilter;

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractQueryableRepository<T, F> setBaseFilter(F filter) {
    this.baseFilter = filter;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public F getBaseFilter() {
    return baseFilter;
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * This implementation creates a {@link RepositoryCriteria} with only the base filter, then
   * delegates to {@link #count(RepositoryCriteria)}.
   * </p>
   */
  @Override
  public int size() {
    RepositoryCriteria<T, F> query = new RepositoryCriteria<>(getBaseFilter());
    return size(query);
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * This implementation creates a {@link RepositoryCriteria} with the current pagination settings,
   * order criteria, and base filter, then delegates to {@link #findBy(RepositoryCriteria)}.
   * </p>
   */
  @Override
  public Stream<T> findAll() {
    RepositoryCriteria<T, F> query =
        new RepositoryCriteria<>(getOffset(), getLimit(), getOrderCriteriaList(), getBaseFilter());
    return findBy(query);
  }
}
