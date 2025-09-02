package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve a predicted text value in a way that
 * makes sense for the specific component. This can be used for implementing text prediction
 * features or auto-complete functionality.
 *
 * <p>
 * For example, a textarea field component may use this interface to provide auto-complete text
 * which appears when the user is typing. The actual logic for generating and updating the predicted
 * value should be implemented by the developer. When the user presses the tab key, or the
 * ArrowRight key, the predicted value will be set to the textarea.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public interface HasPredictedText<T extends Component> {

  /**
   * Retrieves the predicted text property of the component.
   *
   * @return the predicted text of the component.
   */
  public default String getPredictedText() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasPredictedText) {
      return ((HasPredictedText<?>) component).getPredictedText();
    }

    throw new UnsupportedOperationException("The component does not support predicted text");
  }

  /**
   * Sets the predicted text of the component.
   *
   * <p>
   * This property can be used by developers to implement text prediction logic. The actual logic
   * for generating and updating the predicted value should be implemented by the developer.
   * </p>
   *
   * @param predictedText the predicted text to set for the component.
   * @return the component itself.
   */
  public default T setPredictedText(String predictedText) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasPredictedText) {
      ((HasPredictedText<?>) component).setPredictedText(predictedText);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support predicted text");
  }
}
