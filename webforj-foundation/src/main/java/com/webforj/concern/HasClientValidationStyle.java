package com.webforj.concern;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying a component's validation style.
 *
 * <p>
 * This interface provides methods to set and retrieve the validation style for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public interface HasClientValidationStyle<T extends Component> {

  /**
   * The validation style to use when displaying validation messages.
   */
  public enum ValidationStyle {
    /**
     * The validation message will be displayed under the component.
     */
    @SerializedName("inline")
    INLINE,

    /**
     * The validation message will be displayed in a popover.
     */
    @SerializedName("popover")
    POPOVER
  }

  /**
   * Sets the validation style to use when displaying validation messages.
   *
   * @param validationStyle the validation style to use.
   * @return the component.
   */
  public default T setValidationStyle(ValidationStyle validationStyle) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClientValidationStyle) {
      ((HasClientValidationStyle<?>) component).setValidationStyle(validationStyle);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support the validation style property");
  }

  /**
   * Returns the validation style to use when displaying validation messages.
   *
   * @return the validation style to use.
   */
  public default ValidationStyle getValidationStyle() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClientValidationStyle) {
      return ((HasClientValidationStyle<?>) component).getValidationStyle();
    }

    throw new UnsupportedOperationException(
        "The component does not support the validation style property");
  }
}
