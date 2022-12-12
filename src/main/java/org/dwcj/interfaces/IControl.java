package org.dwcj.interfaces;

public interface IControl {

    /**
     * Returns the control's ID
     * 
     * @return Control's ID
     */
    public String getId();

    /**
     * Assigns the ID of a control
     * 
     * @param id Desired ID designation
     * @return The control itself
     */
    public IControl setId(String id);


    /**
     * Allows user to fetch extraneous, user-included information from the control
     *
     * @param key
     * @return Desired piece of user data
     */
    public Object getUserData(String key);

    /**
     * Allows user to include extraneous information in the control
     *
     * @param key Key of the data 
     * @param data Desired piece of information
     * @return The control itself
     */
    public IControl setUserData(String key, Object data);

}
