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

    /**
     * Set the tooltip text of the control
     *
     * @param text
     * @return the control itself
     */
    public IControl setTooltipText(String text);

    /**
     * get the tooltip text property of the control
     *
     * @return the tooltip text
     */
    public String getTooltipText();

    /**
     * Set whether the control is to be enabled
     *
     * @param enabled
     * @return the control itself
     */
    public IControl setEnabled(boolean enabled);

    /**
     *
     * @return if control is enabled (=true) or disabled (=false)
     */
    public boolean isEnabled();

    /**
     * Set whether the control is visible or invisible
     *
     * @param visible
     * @return the control itself
     */
    public IControl setVisible(boolean visible);

    /**
     *
     * @return if control is visible (=true) or invisible (=false)
     */
    public boolean isVisible();

}
