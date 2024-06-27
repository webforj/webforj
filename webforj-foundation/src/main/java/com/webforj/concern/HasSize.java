package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface that allows components to set and retrieve width and height.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.10
 * @author Hyyan Abo Fakher
 */
public interface HasSize<T extends Component> extends HasWidth<T>, HasMinWidth<T>, HasMaxWidth<T>,
    HasHeight<T>, HasMinHeight<T>, HasMaxHeight<T> {

  /**
   * Sets the size of the component.
   *
   * @param width the width to set for the component.
   * @param height the height to set for the component.
   *
   * @return the component itself.
   */
  public default T setSize(String width, String height) {
    setWidth(width);
    setHeight(height);
    return (T) this;
  }

  /**
   * Sets the size of the component.
   *
   * @param width the width to set for the component.
   * @param height the height to set for the component.
   *
   * @return the component itself.
   */
  public default T setSize(float width, float height) {
    setWidth(width);
    setHeight(height);
    return (T) this;
  }

  /**
   * Sets the minimum size of the component.
   *
   * @param minWidth the minimum width to set for the component.
   * @param minHeight the minimum height to set for the component.
   *
   * @return the component itself.
   */
  public default T setMinSize(String minWidth, String minHeight) {
    setMinWidth(minWidth);
    setMinHeight(minHeight);
    return (T) this;
  }

  /**
   * Sets the minimum size of the component.
   *
   * @param minWidth the minimum width to set for the component.
   * @param minHeight the minimum height to set for the component.
   *
   * @return the component itself.
   */
  public default T setMinSize(float minWidth, float minHeight) {
    setMinWidth(minWidth);
    setMinHeight(minHeight);
    return (T) this;
  }

  /**
   * Sets the maximum size of the component.
   *
   * @param maxWidth the maximum width to set for the component.
   * @param maxHeight the maximum height to set for the component.
   *
   * @return the component itself.
   */
  public default T setMaxSize(String maxWidth, String maxHeight) {
    setMaxWidth(maxWidth);
    setMaxHeight(maxHeight);
    return (T) this;
  }

  /**
   * Sets the maximum size of the component.
   *
   * @param maxWidth the maximum width to set for the component.
   * @param maxHeight the maximum height to set for the component.
   *
   * @return the component itself.
   */
  public default T setMaxSize(float maxWidth, float maxHeight) {
    setMaxWidth(maxWidth);
    setMaxHeight(maxHeight);
    return (T) this;
  }
}
