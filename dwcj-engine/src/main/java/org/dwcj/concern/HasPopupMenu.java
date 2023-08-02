package org.dwcj.concern;

/**
 * On applicable controls, helps facilitate underlying BBj popup menu behavior to facilitate
 * implementation of methods to interact with this behavior.
 */

public interface HasPopupMenu {

  HasPopupMenu addPopupMenu(int id, String title);

  HasPopupMenu removePopupMenu(int id, String title);
}
