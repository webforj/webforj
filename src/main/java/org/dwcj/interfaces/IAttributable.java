package org.dwcj.interfaces;

public interface IAttributable {
    
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
    public IAttributable setAttribute(String attribute, String value);
}
