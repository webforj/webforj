package com.webforj.concern;

/**
 * An interface for components that expose a localization bundle.
 *
 * <p>
 * The bundle type {@code I} is component specific. Each component picks the bundle that carries its
 * user visible strings.
 * </p>
 *
 * @param <T> the type of the component that implements this interface
 * @param <I> the type of the i18n bundle
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasI18n<T, I> {

  /**
   * Sets the i18n bundle.
   *
   * @param i18n the i18n bundle
   * @return the component itself
   */
  T setI18n(I i18n);

  /**
   * Gets the i18n bundle.
   *
   * @return the i18n bundle
   */
  I getI18n();
}
