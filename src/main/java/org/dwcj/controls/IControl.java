package org.dwcj.controls;

public interface IControl {

    /**
     * get the text property of the control
     *
     * @return the text
     */
    public String getText();

    /**
     * set the text of the control
     * Each control has a text property, which might be visible in different ways (caption, title, contents of edit) or sometimes not visible at all
     *
     * @param text
     * @return the control itself
     */
    public IControl setText(String text);

    /**
     * set an attribute value
     *
     * @param attribute the key/name of the attribute
     * @param value     the value
     * @return the control itself
     */
    public IControl setAttribute(String attribute, String value);

    /**
     * retrieve the value of the given attribute
     *
     * @param attribute the key/name of the attribute
     * @return the value
     */
    public String getAttribute(String attribute);

}
