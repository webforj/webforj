package com.webforj.router;

import com.webforj.router.history.History;
import com.webforj.router.history.Location;

/**
 * Options for navigating to a new location.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class NavigationOptions {

  /**
   * Describes the navigation type to perform when navigating to a new location.
   */
  public enum NavigationType {
    /**
     * Push the new location to the history stack.
     */
    PUSH,
    /**
     * Replace the current location with the new location.
     */
    REPLACE
  }

  private NavigationType navigationType = NavigationType.PUSH;
  private boolean fireEvents = true;
  private boolean invokeObservers = true;
  private boolean updateHistory = true;
  private Object state = null;

  /**
   * Sets the navigation type.
   *
   * <p>
   * The default navigation type is {@link NavigationType#PUSH}. This means that the new location
   * will be pushed to the history stack and the current location will be preserved. If the
   * navigation type is set to {@link NavigationType#REPLACE}, the current location will be replaced
   * with the new location and the history stack will not be affected.
   * </p>
   *
   * @param navigationType the navigation type
   * @return the navigate options
   */
  public NavigationOptions setNavigationType(NavigationType navigationType) {
    this.navigationType = navigationType;
    return this;
  }

  /**
   * Gets the navigation type.
   *
   * @return the navigation type
   * @see #setNavigationType(NavigationType)
   */
  public NavigationType getNavigationType() {
    return navigationType;
  }

  /**
   * Sets whether to fire events.
   *
   * <p>
   * The default value is {@code true}. If set to {@code false}, the navigation lifecycle events
   * will not be fired.
   * </p>
   *
   * @param fireEvents whether to fire events
   * @return the navigate options
   */
  public NavigationOptions setFireEvents(boolean fireEvents) {
    this.fireEvents = fireEvents;
    return this;
  }

  /**
   * Gets whether to fire events.
   *
   * @return whether to fire events
   * @see #setFireEvents(boolean)
   */
  public boolean isFireEvents() {
    return fireEvents;
  }

  /**
   * Sets whether to invoke observers.
   *
   * <p>
   * The default value is {@code true}. If set to {@code false}, the navigation observers
   * implemented by the components will not be invoked.
   * </p>
   *
   * @param invokeObservers whether to invoke observers
   * @return the navigate options
   */
  public NavigationOptions setInvokeObservers(boolean invokeObservers) {
    this.invokeObservers = invokeObservers;
    return this;
  }

  /**
   * Gets whether to invoke observers.
   *
   * @return whether to invoke observers
   * @see #setInvokeObservers(boolean)
   */
  public boolean isInvokeObservers() {
    return invokeObservers;
  }

  /**
   * Sets whether to update the history location.
   *
   * <p>
   * The default value is {@code true}. If set to {@code false}, the history location will not be
   * updated. Note that setting this to false will not prevent the route from firing events or
   * invoking observers of navigated components.
   * </p>
   *
   * @param updateHistoryLocation whether to update the history location
   * @return the navigate options
   */
  public NavigationOptions setUpdateHistory(boolean updateHistoryLocation) {
    this.updateHistory = updateHistoryLocation;
    return this;
  }

  /**
   * Gets whether to update the history location.
   *
   * @return whether to update the history location
   * @see #setUpdateHistory(boolean)
   */
  public boolean isUpdateHistory() {
    return updateHistory;
  }

  /**
   * Sets the state object.
   *
   * <p>
   * The state object is passed to {@link History#pushState(Object, Location)} or
   * {@link History#replaceState(Object, Location)} when updating the history location.
   * </p>
   *
   * @param state the state object
   * @return the navigate options
   */
  public NavigationOptions setState(Object state) {
    this.state = state;
    return this;
  }

  /**
   * Gets the state object.
   *
   * @return the state object
   * @see #setState(Object)
   */
  public Object getState() {
    return state;
  }
}
