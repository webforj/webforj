package org.dwcj.interfaces;

public interface ICssClassable {
    
    /**
     * Adds a CSS class to the list of CSS classes for the control.
     * 
     * @return The control itself
     */
    public ICssClassable addClass(String selector);

    /**
     * Removes a CSS class from the list of CSS classes for the control.
     *
     * @return The control itself
     */
    public ICssClassable removeClass(String selector);
}
