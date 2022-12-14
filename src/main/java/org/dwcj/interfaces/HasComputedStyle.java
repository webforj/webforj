package org.dwcj.interfaces;

/**
 * Returns a String containing the values of all CSS properties of an element, after 
 * applying active stylesheets and resolving any basic computation those values may contain.
 * 
 * @return String containing all computed styles
 */

public interface HasComputedStyle {

    public String getComputedStyle(String property);
    
}
