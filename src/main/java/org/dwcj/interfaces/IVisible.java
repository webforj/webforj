package org.dwcj.interfaces;

public interface IVisible {
    
    /**
     *
     * @return if control is visible (=true) or invisible (=false)
     */
    public Boolean isVisible();

    /**
     * Set whether the control is visible or invisible
     *
     * @param visible
     * @return the control itself
     */
    public IVisible setVisible(Boolean visible);
}
