package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying a component's pattern.
 *
 * <p>
 * This interface provides methods to set and retrieve the pattern for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @since 24.01
 */
public interface HasPattern<T extends Component> {

  /**
   * Specifies a regular expression the form component's value should match.
   *
   * <p>
   * The pattern attribute, when specified, is a regular expression that the component's value must
   * match in order for the value to pass constraint validation. It must be a valid JavaScript
   * regular expression, as used by the RegExp type, and as documented in the
   * <a href= "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions">
   * regular expressions guide; </a> For more information about patterns see
   * <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/pattern#overview"> this
   * guide.</a>
   * </p>
   *
   * @param pattern the pattern to set
   * @return the component itself after setting the pattern.
   */
  public default T setPattern(String pattern) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasPattern) {
      ((HasPattern<?>) component).setPattern(pattern);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support the pattern attribute");
  }

  /**
   * Retrieves the pattern for the component.
   *
   * @return the pattern for the component.
   * @see #setPattern(String)
   */
  public default String getPattern() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasPattern) {
      return ((HasPattern<?>) component).getPattern();
    }

    throw new UnsupportedOperationException("The component does not support the pattern attribute");
  }
}
