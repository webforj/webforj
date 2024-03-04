package com.webforj.data.repository;

/**
 * An interface for components that have a repository.
 *
 * @param <T> the type of the component
 * @param <F> the type of the filter
 *
 * @see Repository
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface HasRepository<T> {

  /**
   * Gets the repository of the component.
   *
   * @return the repository
   */
  Repository<T> getRepository();

  /**
   * Sets the repository of the component.
   *
   * @param repository the repository
   */
  HasRepository<T> setRepository(Repository<T> repository);
}
