package com.webforj.automation.see.component.appnav;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.layout.appnav.AppNavItem;

/**
 * Serializes a navigation item with its path.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class AppNavItemSnapshotSerializer implements SnapshotSerializer<AppNavItem> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<AppNavItem> getContract() {
    return AppNavItem.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(AppNavItem target, SnapshotNode node) {
    node.setAttribute("path", target.getPath());
  }
}
