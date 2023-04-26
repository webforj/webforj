package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.window.AbstractWindow;

/**
 * Abstract base class for all engine controls. Implements default behaviors for the implemented
 * interface methods. Extended by AbstractDwcControl.
 */

public abstract class AbstractComponent implements Component, HasDestroy {

  /*
   * The id of the component.
   */
  private String uuid = "";

  /*
   * Underlying BBj control
   */
  protected BBjControl ctrl;

  /*
   * Members responsible for tracking ID attribute and user data
   */
  protected final Map<String, Object> userData = new HashMap<>();

  /*
   * Used by catchUp() method to ensure a single execution of the function.
   */
  private Boolean caughtUp = false;

  /*
   * Used to track whether or not the control that has been flagged for destruction
   */
  protected Boolean destroyed = false;

  static {
    ComponentAccessor.setDefault(new ComponentAccessorImpl());
  }

  /**
   * Create the object on a panel p. The preferred way of creating an object is using the
   * Panel::add(Control) method, instead of this
   *
   * @param panel the panel to add this control on
   */
  protected abstract void create(AbstractWindow panel);

  /**
   * This method gets the underlying original BBj control It's package private and can only be
   * accessed through the ControlAccessor No API user / customer shall ever work directly with BBj
   * controls.
   *
   * @return the underlying BBj control
   */
  BBjControl getControl() {
    return this.ctrl;
  }

  public Object getUserData(String key) {
    return this.userData.get(key);
  }

  public AbstractComponent setUserData(String key, Object data) {
    this.userData.put(key, data);
    return this;
  }

  /**
   *{@inheritDoc}
   */
  @Override
  public String getComponentId() {
    if (uuid.equals("")) {
      uuid = UUID.randomUUID().toString();
    }
    return uuid;
  }

  public Boolean getCaughtUp() {
    return this.caughtUp;
  }

  /**
   * The catchUp method is used to replay attributes and settings that the API user might have added
   * to a control before its creation. A control is not created before it's added to a panel.
   * Anything that is added between instantiation of a control and its addition to a panel has to be
   * recorded and replayed in this method
   *
   * @throws IllegalAccessException - thrown if an attempt is made to call this method more than
   *         once
   */
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {

    if (Boolean.TRUE.equals(this.caughtUp)) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }

    this.caughtUp = true;
  }

  @Override
  public void destroy() {
    this.destroyed = true;
  }

  @Override
  public Boolean isDestroyed() {
    return this.destroyed;
  }
}
