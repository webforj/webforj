package org.dwcj.interfaces;


/**
 * Interface which facilitates controls that implement it the ability to 
 * set the text of the control, if applicable, in a way that makes 
 * sense for the specific control itself.
 */
public interface HasControlText {

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
    public HasControlText setText(String text);
    
}
