package org.dwcj.interfaces;

public interface ITooltipable {

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
    public ITooltipable setTooltipText(String tooltipText);
    
}
