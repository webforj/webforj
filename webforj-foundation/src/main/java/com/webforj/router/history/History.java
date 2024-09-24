package com.webforj.router.history;

import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.history.event.HistoryStateChangeEvent;
import java.util.Optional;

/**
 * Represents the session history.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public interface History {

  /**
   * Goes to the previous page in session history.
   *
   * @return the history object
   */
  History back();

  /**
   * Goes to the next page in session history.
   *
   * @return the history object
   */
  History forward();

  /**
   * Goes to the specified page in session history.
   *
   * @param index the index of the page in session history
   * @return the history object
   */
  History go(int index);

  /**
   * Returns the number of pages in the history stack.
   *
   * @return the number of pages in the history stack
   */
  public int size();

  /**
   * Returns the current location.
   *
   * @return the current location
   */
  Optional<Location> getLocation();

  /**
   * Pushes the given state object onto the session history stack with the specified location.
   *
   * @param state the state object
   * @param location the location
   *
   * @return the history object
   */
  History pushState(Object state, Location location);

  /**
   * Pushes the given location onto the session history stack with a null state object.
   *
   * @param location the location
   * @return the history object
   */
  default History pushState(Location location) {
    return pushState(null, location);
  }

  /**
   * Replaces the current state object with the given state object and the specified location.
   *
   * @param state the state object
   * @param location the location
   *
   * @return the history object
   */
  History replaceState(Object state, Location location);

  /**
   * Replaces the current state object with a null state object and the specified location.
   *
   * @param location the location
   * @return the history object
   */
  default History replaceState(Location location) {
    return replaceState(null, location);
  }

  /**
   * Adds a listener to be notified when the history state changes.
   *
   * @param listener the listener
   * @return the listener registration
   */
  ListenerRegistration<HistoryStateChangeEvent> addHistoryStateChangeListener(
      EventListener<HistoryStateChangeEvent> listener);

  /**
   * Alias for {@link #addHistoryStateChangeListener(HistoryStateChangeEvent)}.
   *
   * @param listener the listener
   * @return the listener registration
   */
  default ListenerRegistration<HistoryStateChangeEvent> onHistoryStateChange(
      EventListener<HistoryStateChangeEvent> listener) {
    return addHistoryStateChangeListener(listener);
  }
}
