package com.webforj.data.concern;

/**
 * An interface for components that support value change modes.
 *
 * <p>
 * This interface provides methods to set and retrieve the value change mode for the component.
 * </p>
 *
 * <p>
 * The interface does not specify the behavior when the value change mode is altered after a
 * component has registered a listener. The responsibility to manage this scenario lies with the
 * implementor. Options include disregarding the change, detaching and then reattaching the listener
 * with the updated value change mode, or applying the new mode only to future listener
 * registrations, leaving existing ones unaffected. webforJ core components typically apply the new
 * mode to future registrations only leaving existing listeners unaffected.
 * </p>
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface ValueChangeModeAware<T> {
  /**
   * The value change mode.
   */
  public enum ValueChangeMode {
    /**
     * The value change mode is when the user leaves the component, typically by clicking outside of
     * it.
     */
    ON_BLUR,

    /**
     * The value change mode is when the user modifies the value of the component.
     */
    ON_MODIFY,
  }

  /**
   * Sets the value change mode.
   *
   * @param mode the value change mode.
   * @return the component itself.
   */
  public T setValueChangeMode(ValueChangeMode mode);

  /**
   * Gets the value change mode.
   *
   * @return the value change mode.
   */
  public ValueChangeMode getValueChangeMode();
}
