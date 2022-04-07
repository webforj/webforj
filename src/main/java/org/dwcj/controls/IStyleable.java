package org.dwcj.controls;

public interface IStyleable {

    /**
     * set an HTML style property
     *
     * @param property the property, e.g. "color"
     * @param value    the property value, e.g. "red"
     * @return the control itself
     */
    IStyleable setStyle(String property, String value);

    //todo: add methods for removing and getting styles

    /**
     * set a CSS class name to a control
     *
     * @param selector the class name
     * @return the control itself
     */
    IStyleable addClass(String selector);

    /**
     * remove a class selector from the control
     *
     * @param selector the class selector to be removed
     * @return the control itself
     */
    IStyleable removeClass(String selector);

}
