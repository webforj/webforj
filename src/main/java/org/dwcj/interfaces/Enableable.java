package org.dwcj.interfaces;


/**
 * Interface that facilitates implementation of behavior which
 * determines whether or not a control can be 
 * disabled after being rendered to the page
 */
public interface Enableable {
    
    /**
     *
     * @return if control is enabled (=true) or disabled (=false)
     */
    public Boolean isEnabled();

    /**
     * Set whether the control is to be enabled
     *
     * @param enabled tells if the control should be enabled (true) or disabled (false)
     * @return the control itself
     */
    public Enableable setEnabled(Boolean enabled);
}
