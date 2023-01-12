package org.dwcj.interfaces;

public interface HasDestroy {

    /**
     * Overridden in various controls to implement destruction of a control, or prevent its addition to a panel
     */
    public void destroy();

    /**
     * Returns whether or not the control is flagged for deletion
     * @return True if the control will be deleted, false otherwise
     */
    public Boolean isDestroyed();
}
