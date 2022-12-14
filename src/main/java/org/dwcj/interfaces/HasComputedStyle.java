package org.dwcj.interfaces;


/**
 * Interface that facilitates the retrieval of CSS properties of a control after 
 * any stylesheet and other computational values have been applied
 */
public interface HasComputedStyle {
    
    /**
     * Returns a String containing the values of all CSS properties of an element, after 
     * applying active stylesheets and resolving any basic computation those values may contain.
     * 
     * @return String containing all computed styles
     */
    public String getComputedStyle(String property);
    
}
