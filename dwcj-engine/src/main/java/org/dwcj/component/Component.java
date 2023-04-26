package org.dwcj.component;

/**
 * Interface used to include all common properties of new controls/components that will be added to
 * the engine, but exclude some of the more specific properties not common across every control.
 */
public interface Component {

  /**
   * Allows user to fetch extraneous, user-included information from the control.
   *
   * @param key Key of the data
   * @return Desired piece of user data
   */
  public Object getUserData(String key);

  /**
   * Allows user to include extraneous information in the control.
   *
   * @param key Key of the data
   * @param data Desired piece of information
   * @return The control itself
   */
  public Component setUserData(String key, Object data);

  /**
   * Allows user to fetch the componentId which is a uuid generated for the specific component.
   * This ID is randomly generated each time a new session is run, and is reliable for querying
   * components in the same session, but not across different sessions.
   *
   * @return The componentId as a String.
   */
  public String getComponentId();
}
