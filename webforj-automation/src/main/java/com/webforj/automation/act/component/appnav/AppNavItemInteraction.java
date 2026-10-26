package com.webforj.automation.act.component.appnav;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ClickInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.component.layout.appnav.AppNavItem;
import com.webforj.component.layout.appnav.event.AppNavLocationChangedEvent;
import com.webforj.component.layout.appnav.event.AppNavPinEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Clicks, pins and unpins the items of navigations, where a click on an item with a path makes the
 * navigation report the location.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class AppNavItemInteraction extends AbstractComponentInteraction<AppNavItem>
    implements ClickInteraction<AppNavItem> {

  /**
   * Creates the interaction.
   */
  public AppNavItemInteraction() {
    super(AppNavItem.class);
  }

  /**
   * Returns the interaction that pins an item of a navigation. The item takes the pin, then the
   * navigation reports it with the keys of every pinned item.
   *
   * <pre>{@code
   * UserInteraction.perform(ordersItem, AppNavItemInteraction.pin())
   * }</pre>
   *
   * @return the interaction to perform on the item
   * @throws InteractionRejectedException when performed, if the item is not inside a navigation,
   *         the navigation does not pin items, or the item is a group header or has no path
   */
  public static Consumer<AppNavItem> pin() {
    return item -> setPinned(item, true);
  }

  /**
   * Returns the interaction that unpins an item of a navigation.
   *
   * @return the interaction to perform on the item
   * @throws InteractionRejectedException when performed, if the item is not inside a navigation,
   *         the navigation does not pin items, or the item is a group header or has no path
   */
  public static Consumer<AppNavItem> unpin() {
    return item -> setPinned(item, false);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void click(AppNavItem item, MouseOptions options) {
    AppNav nav = findNav(item);
    String path = item.getFullPath();
    if (path == null || path.isBlank()) {
      return;
    }

    Map<String, Object> data = new HashMap<>();
    data.put("path", path);
    fire(new AppNavLocationChangedEvent(nav, data));
  }

  private static void setPinned(AppNavItem item, boolean pinned) {
    AppNav nav = findNav(item);
    if (!nav.getPinning().isEnabled()) {
      throw reject(item, "is inside a navigation without pinning and a user cannot pin it");
    }

    if (!item.getItems().isEmpty()) {
      throw reject(item, "is a group header and a user cannot pin it");
    }

    String path = item.getFullPath();
    if (path == null || path.isBlank()) {
      throw reject(item, "has no path and a user cannot pin it");
    }

    List<String> keys = new ArrayList<>();
    for (AppNavItem pinnedItem : nav.getPinning().getItems()) {
      if (pinnedItem != item) {
        keys.add(pinnedItem.getPinKey());
      }
    }

    if (pinned) {
      keys.add(item.getPinKey());
    }

    item.setPinned(pinned);

    Map<String, Object> data = new HashMap<>();
    data.put("key", item.getPinKey());
    data.put("pinned", pinned);
    data.put("keys", keys);
    fire(new AppNavPinEvent(nav, data));
  }

  private static AppNav findNav(AppNavItem item) {
    return findHolder(item, AppNav.class)
        .orElseThrow(() -> reject(item, "is not inside a navigation"));
  }
}
