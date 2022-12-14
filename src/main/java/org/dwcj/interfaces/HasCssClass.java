package org.dwcj.interfaces;


/**
 * Interface facilitating implementation of behavior to add or remove a 
 * CSS class to a control
 */
public interface HasCssClass {
    
    /**
     * Adds a CSS class to the list of CSS classes for the control.
     * 
     * @return The control itself
     */
    public HasCssClass addClassName(String selector);

    /**
     * Removes a CSS class from the list of CSS classes for the control.
     *
     * @return The control itself
     */
    public HasCssClass removeClassName(String selector);
}
