package org.dwcj.interfaces;

/**
 * Interface facilitating implementation of behavior to modify a specific
 * CSS class to a provided value.
 */
public interface HasCssStyle {
    
    /**
     * Assigns specified value to the specified CSS property
     * 
     * @param property The CSS property to be changed
     * @param value The value of the selected CSS property
     * @return The control itself
     */
    public HasCssStyle setStyle(String property, String value);
}
