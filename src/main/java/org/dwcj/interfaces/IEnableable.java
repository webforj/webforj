package org.dwcj.interfaces;

public interface IEnableable {
    
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
    public IEnableable setEnabled(Boolean enabled);
}
