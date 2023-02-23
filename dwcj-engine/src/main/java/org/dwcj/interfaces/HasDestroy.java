package org.dwcj.interfaces;

public interface HasDestroy {

    /**
     * Destroy the control and remove it from the panel
     */
    public void destroy();

    /**
     * Returns whether or not the control is destroyed
     * @return True if the control will is destroyed, false otherwise
     */
    public Boolean isDestroyed();
}
