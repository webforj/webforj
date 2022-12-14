package org.dwcj.interfaces;


/**
 * Interface which facilitates functionality to be implemented that controls whether
 * or not a control can gain and lose focus.
 */
public interface Focusable {

    Boolean isFocusable();

    Focusable setFocusable(Boolean focusable) ;
    
}
