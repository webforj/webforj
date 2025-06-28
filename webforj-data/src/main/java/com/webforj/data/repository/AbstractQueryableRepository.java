package com.webforj.data.repository;

import java.util.Comparator;
import java.util.Optional;
import java.util.function.Predicate;
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

  /**
   * {@inheritDoc}
   *
   * @throws UnsupportedOperationException if called, as this method is not supported in
   *         {@link AbstractQueryableRepository}.
   * @deprecated Use {@link #findBy(RepositoryCriteria)} instead
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public Stream<T> findBy(RetrievalCriteria<T> criteria) {
    throw new UnsupportedOperationException(
        "findBy(RetrievalCriteria) is deprecated and not supported in "
            + "AbstractQueryableRepository. Use findBy(RepositoryCriteria) instead.");
  }

  /**
   * {@inheritDoc}
   *
   * @throws UnsupportedOperationException if called, as this method is not supported in
   *         {@link AbstractQueryableRepository}.
   * @deprecated This method is designed for in-memory repositories only and will be removed in
   *             version 26.00.
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public Optional<T> findByIndex(int index) {
    throw new UnsupportedOperationException("findByIndex(int) is deprecated and not supported in "
        + "AbstractQueryableRepository. This method is designed for "
        + "in-memory repositories only.");
  }

  /**
   * {@inheritDoc}
   *
   * @throws UnsupportedOperationException if called, as this method is not supported in
   *         {@link AbstractQueryableRepository}.
   * @deprecated This method is designed for in-memory repositories only and will be removed in
   *             version 26.00.
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public int getIndex(T entity) {
    throw new UnsupportedOperationException("getIndex(T) is deprecated and not supported in "
        + "AbstractQueryableRepository. This method is designed for "
        + "in-memory repositories only.");
  }

  /**
   * {@inheritDoc}
   *
   * @throws UnsupportedOperationException if called, as this method is not supported in
   *         {@link AbstractQueryableRepository}.
   * @deprecated Use {@link #setBaseFilter(Object)} instead
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public AbstractQueryableRepository<T, F> setFilter(Predicate<T> filter) {
    throw new UnsupportedOperationException(
        "setFilter(Predicate) is deprecated and not supported in "
            + "AbstractQueryableRepository. Use setBaseFilter(F) with your "
            + "custom filter type instead.");
  }

  /**
   * {@inheritDoc}
   *
   * @throws UnsupportedOperationException if called, as this method is not supported in
   *         {@link AbstractQueryableRepository}.
   * @deprecated Use {@link #getBaseFilter()} instead
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public Predicate<T> getFilter() {
    throw new UnsupportedOperationException("getFilter() is deprecated and not supported in "
        + "AbstractQueryableRepository. Use getBaseFilter() which returns "
        + "your custom filter type instead.");
  }

  /**
   * {@inheritDoc}
   *
   * @throws UnsupportedOperationException if called, as this method is not supported in
   *         {@link AbstractQueryableRepository}.
   * @deprecated Use {@link OrderCriteriaList} instead
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public AbstractQueryableRepository<T, F> setOrderBy(Comparator<T> orderBy) {
    throw new UnsupportedOperationException(
        "setOrderBy(Comparator) is deprecated and not supported in "
            + "AbstractQueryableRepository. Use getOrderCriteriaList().add(OrderCriteria) "
            + "instead.");
  }

  /**
   * {@inheritDoc}
   *
   * @throws UnsupportedOperationException if called, as this method is not supported in
   *         {@link AbstractQueryableRepository}.
   * @deprecated Use {@link #getOrderCriteriaList()} instead
   */
  @Override
  @Deprecated(since = "25.02", forRemoval = true)
  public Comparator<T> getOrderBy() {
    throw new UnsupportedOperationException("getOrderBy() is deprecated and not supported in "
        + "AbstractQueryableRepository. Use getOrderCriteriaList() which "
        + "returns OrderCriteriaList<T> instead.");
  }
}
