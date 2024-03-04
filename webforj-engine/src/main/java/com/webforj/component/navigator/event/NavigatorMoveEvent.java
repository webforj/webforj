package com.webforj.component.navigator.event;

import com.webforj.component.event.ComponentEvent;
import com.webforj.component.navigator.Navigator;
import java.util.Map;

/**
 * A base class for all navigator events.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public abstract class NavigatorMoveEvent extends ComponentEvent<Navigator> {
  private int current;
  private int startIndex;
  private int endIndex;

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  protected NavigatorMoveEvent(Navigator component, Map<String, Object> eventMap) {
    super(component, eventMap);
    this.current = Math.max((int) eventMap.get("current"), 1);
    this.startIndex = Math.max((int) eventMap.get("startIndex"), 0);
    this.endIndex = Math.max((int) eventMap.get("endIndex"), 0);
  }

  /**
   * Returns the current page.
   *
   * @return the current page
   */
  public int getCurrent() {
    return current;
  }

  /**
   * Returns the start index of the items on the current page.
   *
   * @return the start index
   */
  public int getStartIndex() {
    return startIndex;
  }

  /**
   * Returns the end index of the items on the current page.
   *
   * @return the end index
   */
  public int getEndIndex() {
    return endIndex;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Navigator getComponent() {
    return (Navigator) super.getComponent();
  }
}
