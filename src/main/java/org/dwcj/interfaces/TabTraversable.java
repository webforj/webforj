package org.dwcj.interfaces;

/**
 * Interface facilitating the implementation of behavior to 
 * access and mutate a control's tab traversal on a rendered page.
 */
public interface TabTraversable {
    
    Boolean isTabTraversable();

    TabTraversable setTabTraversable(Boolean traversable) ;
}
