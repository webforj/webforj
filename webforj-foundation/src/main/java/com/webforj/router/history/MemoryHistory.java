package com.webforj.router.history;

import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.history.event.HistoryStateChangeEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Memory-based implementation of the History interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class MemoryHistory implements History {
  private EventDispatcher dispatcher = new EventDispatcher();
  private final List<Entry> history;
  private int currentIndex;

  /**
   * Constructs an empty MemoryHistory.
   */
  public MemoryHistory() {
    this.history = new ArrayList<>();
    this.currentIndex = -1;
  }

  /**
   * Constructs an empty MemoryHistory.
   *
   * @param history the history
   * @param currentIndex the current index
   */
  public MemoryHistory(List<Entry> history, int currentIndex) {
    this.history = new ArrayList<>(history);
    this.currentIndex = currentIndex;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History back() {
    if (currentIndex > 0) {
      currentIndex--;
      fireHistoryStateChangeEventForIndex(currentIndex);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History forward() {
    if (currentIndex < history.size() - 1) {
      currentIndex++;
      fireHistoryStateChangeEventForIndex(currentIndex);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History go(int index) {
    if (index >= 0 && index < history.size()) {
      currentIndex = index;
      fireHistoryStateChangeEventForIndex(currentIndex);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int size() {
    return history.size();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getLocation() {
    if (currentIndex >= 0 && currentIndex < history.size()) {
      return Optional.of(history.get(currentIndex).getLocation());
    } else {
      return Optional.empty();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History pushState(Object state, Location location) {
    if (currentIndex < history.size() - 1) {
      history.subList(currentIndex + 1, history.size()).clear();
    }

    history.add(new Entry(state, location));
    currentIndex++;
    fireHistoryStateChangeEvent(location, state);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History replaceState(Object state, Location location) {
    if (currentIndex >= 0 && currentIndex < history.size()) {
      history.set(currentIndex, new Entry(state, location));
    } else {
      pushState(state, location);
    }

    fireHistoryStateChangeEvent(location, state);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<HistoryStateChangeEvent> addHistoryStateChangeListener(
      EventListener<HistoryStateChangeEvent> listener) {
    return getDispatcher().addListener(HistoryStateChangeEvent.class, listener);
  }

  private void fireHistoryStateChangeEvent(Location location, Object state) {
    HistoryStateChangeEvent event = new HistoryStateChangeEvent(this, location, state);
    getDispatcher().dispatchEvent(event);
  }

  private void fireHistoryStateChangeEventForIndex(int index) {
    if (index >= 0 && index < history.size()) {
      Entry entry = history.get(index);
      fireHistoryStateChangeEvent(entry.getLocation(), entry.getState());
    }
  }

  EventDispatcher getDispatcher() {
    return dispatcher;
  }

  /**
   * Represents an entry in the memory history stack.
   */
  private static final class Entry {
    private final Object state;
    private final Location location;

    public Entry(Object state, Location location) {
      this.state = state;
      this.location = location;
    }

    public Object getState() {
      return state;
    }

    public Location getLocation() {
      return location;
    }
  }
}