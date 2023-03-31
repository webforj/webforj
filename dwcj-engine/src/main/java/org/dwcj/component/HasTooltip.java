package org.dwcj.component;


/**
 * On applicable controls, helps facilitate tooltip functionality
 * to facilitate implementation of methods
 * to interact with this behavior.
 */
public interface HasTooltip {

    /**
     * get the tooltip text property of the control
     *
     * @return the tooltip text
     */
    public String getTooltipText();

    /**
     * Set the tooltip text of the control
     *
     * @param text
     * @return the control itself
     */
    public HasTooltip setTooltipText(String tooltipText);
    
}
