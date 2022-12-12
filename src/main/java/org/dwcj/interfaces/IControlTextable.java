package org.dwcj.interfaces;

public interface IControlTextable {

    /**
     * get the text property of the control
     *
     * @return the text
     */
    public String getText();

    /**
     * set the text of the control
     * Each control implementing this interface has a text property, which might be visible in different ways (caption, title, contents of edit) or sometimes not visible at all
     *
     * @param text
     * @return the control itself
     */
    public IControlTextable setText(String text);
    
}
