package com.webforj.router.history.event;

import com.webforj.router.history.History;
import com.webforj.router.history.Location;
import java.util.EventObject;
import java.util.Optional;

/**
 * Represents a history state change event.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class HistoryStateChangeEvent extends EventObject {
  private final Location location;
  private final transient Object state;

  /**
   * Constructs a new HistoryStateChangeEvent object.
   *
   * @param source the history object that fired the event
   * @param location the location
   * @param state the state object
   */
  public HistoryStateChangeEvent(History source, Location location, Object state) {
    super(source);
    this.location = location;
    this.state = state;
  }

  /**
   * Returns the history object that fired the event.
   *
   * @return the history object
   */
  public History getHistory() {
    return (History) getSource();
  }

  /**
   * Returns the location.
   *
   * @return the location
   */
  public Optional<Location> getLocation() {
    return Optional.ofNullable(location);
  }

  /**
   * Returns the state object.
   *
   * @return the state object
   */
  public Optional<Object> getState() {
    return Optional.ofNullable(state);
  }
}
