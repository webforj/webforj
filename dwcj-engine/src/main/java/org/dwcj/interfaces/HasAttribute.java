package org.dwcj.interfaces;

/**
 * Interface which facilitates implementation of behavior that allows
 * for access and mutation of HTML attributes on a control
 */
public interface HasAttribute {
    
    /**
     * retrieve the value of the given attribute
     *
     * @param attribute the key/name of the attribute
     * @return the value
     */
    public String getAttribute(String attribute);

    /**
     * set an attribute value
     *
     * @param attribute the key/name of the attribute
     * @param value     the value
     * @return the control itself
     */
    public HasAttribute setAttribute(String attribute, String value);
}
