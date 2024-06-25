package com.webforj.component.field;

/**
 * Represents a field that can be spun in a spinner.
 *
 * @param <T> The type of the field itself.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
interface Spinnable<T> {

  /**
   * Spin in the spinner in the forward direction.
   *
   * @return This component itself.
   */
  public T spinUp();

  /**
   * Spin in the spinner in the backward direction.
   *
   * @return This component itself.
   */
  public T spinDown();
}
