package org.dwcj.interfaces;


/**
 * Interface facilitates implementation of behaviors to modify a 
 * controls visibility on a page
 */
public interface Visible {
    
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
    public Visible setVisible(Boolean visible);
}
