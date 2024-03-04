package com.webforj.component.event;

import com.webforj.component.DwcComponent;
import java.util.Map;

/**
 * A component event which is fired when a DWC component executes an async javascript script.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ExecuteAsyncScriptEvent extends ComponentEvent<DwcComponent<?>> {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param payload the event map
   */
  public ExecuteAsyncScriptEvent(DwcComponent<?> component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Get the index of the async script.
   *
   * @return the index
   */
  public int getIndex() {
    return Integer.parseInt(getEventMap().get("index") + "");
  }

  /**
   * Get the result of the async script.
   *
   * <p>
   * When returning values from JavaScript to Java, the following conversions are applied:
   * <ul>
   * <li>JavaScript numbers are converted to {@code java.lang.Integer}, {@code java.lang.Long}, or
   * {@code java.lang.Double}.</li>
   * <li>JavaScript strings are converted to {@code java.lang.String}.</li>
   * <li>JavaScript booleans are converted to {@code java.lang.Boolean}.</li>
   * <li>JavaScript null and undefined are converted to {@code null}.</li>
   * <li>All other types are converted to a string representation.</li>
   * </ul>
   * </p>
   *
   * @return the result
   */

  public Object getResult() {
    return getEventMap().get("result");
  }
}

