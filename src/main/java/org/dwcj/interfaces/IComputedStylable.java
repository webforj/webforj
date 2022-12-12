package org.dwcj.interfaces;

public interface IComputedStylable {

    /**
     * Returns a String containing the values of all CSS properties of an element, after 
     * applying active stylesheets and resolving any basic computation those values may contain.
     * 
     * @return String containing all computed styles
     */
    public String getComputedStyle(String property);
    
}
