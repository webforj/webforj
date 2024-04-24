package com.webforj.data;

/**
 * Interface for classes that are aware of a bean class.
 *
 * @param <B> the type of the bean class.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface BeanAware<B> {

  /**
   * Sets the bean class.
   */
  public void setBeanClass(Class<B> beanClass);

  /**
   * Gets the bean class.
   *
   * @return the bean class.
   */
  public Class<B> getBeanClass();
}
