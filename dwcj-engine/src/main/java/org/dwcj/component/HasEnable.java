package org.dwcj.component;


/**
 * Interface that facilitates implementation of behavior which
 * determines whether or not a control can be 
 * disabled after being rendered to the page
 */
public interface HasEnable {
    
    /**
     *
     * @return if control is enabled (=true) or disabled (=false)
     */
    public Boolean isEnabled();

    /**
     * Set whether the control is to be enabled
     *
     * @param enabled
     * @return the control itself
     */
    public HasEnable setEnabled(Boolean enabled);
}
