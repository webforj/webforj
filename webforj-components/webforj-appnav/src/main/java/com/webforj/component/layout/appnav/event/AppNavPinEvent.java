package com.webforj.component.layout.appnav.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.component.layout.appnav.AppNavItem;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Emitted when an item's pinned state changes.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 *
 * @see AppNav
 */
@EventName(value = "dwc-pin-changed")
@EventOptions(data = {@EventData(key = "key", exp = "event.detail.key"),
    @EventData(key = "pinned", exp = "event.detail.pinned"),
    @EventData(key = "keys", exp = "event.detail.keys")})
public final class AppNavPinEvent extends ComponentEvent<AppNav> {

  /**
   * Creates a new {@code AppNavPinEvent} with the given target and detail.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppNavPinEvent(AppNav target, Map<String, Object> detail) {
    super(target, detail);
  }

  /**
   * Gets the key of the item whose pinned state changed.
   *
   * @return the key
   */
  public String getKey() {
    return (String) getData().get("key");
  }

  /**
   * Gets the item whose pinned state changed.
   *
   * <p>
   * Resolves the item from the navigation by matching its {@link AppNavItem#getPinKey() pin key}.
   * Returns {@code null} when the item is no longer part of the navigation.
   * </p>
   *
   * @return the item whose pinned state changed, or {@code null} when it is no longer part of the
   *         navigation
   */
  public AppNavItem getItem() {
    return findByPinKey(((AppNav) getComponent()).getItems(), getKey()).orElse(null);
  }

  /**
   * Gets whether the item is now pinned.
   *
   * @return {@code true} if the item is now pinned, {@code false} otherwise
   */
  public boolean isPinned() {
    return Boolean.TRUE.equals(getData().get("pinned"));
  }

  /**
   * Gets the full set of currently pinned keys, in pinned order.
   *
   * @return the pinned keys
   */
  @SuppressWarnings("unchecked")
  public List<String> getKeys() {
    Object keys = getData().get("keys");

    return keys instanceof List ? (List<String>) keys : List.of();
  }

  private Optional<AppNavItem> findByPinKey(List<AppNavItem> items, String key) {
    for (AppNavItem item : items) {
      if (Objects.equals(item.getPinKey(), key)) {
        return Optional.of(item);
      }

      Optional<AppNavItem> found = findByPinKey(item.getItems(), key);
      if (found.isPresent()) {
        return found;
      }
    }

    return Optional.empty();
  }
}
