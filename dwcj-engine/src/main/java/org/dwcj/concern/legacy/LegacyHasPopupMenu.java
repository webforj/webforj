package org.dwcj.concern.legacy;

/**
 * On applicable controls, helps facilitate underlying BBj popup menu behavior to facilitate
 * implementation of methods to interact with this behavior.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasPopupMenu {

  LegacyHasPopupMenu addPopupMenu(int id, String title);

  LegacyHasPopupMenu removePopupMenu(int id, String title);
}
