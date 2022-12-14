package org.dwcj.interfaces;

/**
 * Interface facilitating implementation of behavior to modify a specific
 * CSS property to a provided value.
 */
public interface HasStyle {
    
    /**
     * Assigns specified value to the specified CSS property
     * 
     * @param property The CSS property to be changed
     * @param value The value of the selected CSS property
     * @return The control itself
     */
    public HasStyle setStyle(String property, String value);
}
