package org.dwcj.component;

/**
 * Interface facilitating the implementation of behavior to 
 * access and mutate a control's tab traversal on a rendered page.
 */
public interface TabTraversable {
    
    /**
     * Returns a boolean indicating whether or not the user can navigate to the control using the tab key.
     * @return True if user can navigate to control with Tab key, False if not
     */
    Boolean isTabTraversable();

    /**
     * Sets whether or not the user can navigate to the control using the tab key.
     * @param traversable Boolean dictating tab traversal. True if control can be navigated to with Tab, False if not.
     * @return The control itself
     */
    TabTraversable setTabTraversable(Boolean traversable) ;
}
