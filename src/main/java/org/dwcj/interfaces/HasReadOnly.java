package org.dwcj.interfaces;

/**
 * Facilitates implementation of methods which allow for the toggle of
 * read only status on a control.
 */
public interface HasReadOnly {
    
    HasReadOnly setReadOnly(Boolean editable);

    Boolean isReadOnly();

}
