package org.dwcj.interfaces;

/**
 * On applicable controls, helps facilitate underlying BBj popup menu
 * behavior to facilitate implementation of methods
 * to interact with this behavior.
 * 
 * ==TODO==
 */

public interface HasPopupMenu {

    HasPopupMenu addPopupMenu(int id, String title);

    HasPopupMenu removePopupMenu(int id, String title);
}
