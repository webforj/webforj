package org.dwcj.interfaces;

public interface ICssStylable {
    
    /**
     * Assigns specified value to the specified CSS property
     * 
     * @param property The CSS property to be changed
     * @param value The value of the selected CSS property
     * @return The control itself
     */
    public ICssStylable setStyle(String property, String value);
}
