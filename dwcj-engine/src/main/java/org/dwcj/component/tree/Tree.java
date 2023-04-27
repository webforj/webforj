package org.dwcj.component.tree;

import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.util.BBjFunctionalityHelper;

/**
 * A Tree control.
 */
public final class Tree extends AbstractDwcComponent {

  private BBjTree bbjTree;
  private TreeNode root;
  private final EventDispatcher dispatcher = new EventDispatcher();
  private FocusEventSink focusEventSink;
  private BlurEventSink blurEventSink;
  private MouseEnterEventSink mouseEnterEventSink;
  private MouseExitEventSink mouseExitEventSink;
  private RightMouseDownEventSink rightMouseDownEventSink;

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags = 
        BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addTree(flags);
      createEventSinks();
      bbjTree = (BBjTree) ctrl;
      catchUp();
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create Tree", e);
    }
  }

  private void createEventSinks() {
    this.focusEventSink = new FocusEventSink(this, dispatcher);
    this.blurEventSink = new BlurEventSink(this, dispatcher);
    this.mouseEnterEventSink = new MouseEnterEventSink(this, dispatcher);
    this.mouseExitEventSink = new MouseExitEventSink(this, dispatcher);
    this.rightMouseDownEventSink = new RightMouseDownEventSink(this, dispatcher);
  }

  /**
   * Adds a focus event for the Tree component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Tree addFocusListener(EventListener<FocusEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addFocusListener method.
   *
   * @see Tree #addFocusListener(EventListener)
   * @param listener A method to receive the focus event
   * @return the component itself
   */
  public Tree onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a focus event from the Tree component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Tree removeFocusListener(EventListener<FocusEvent> listener) {
    dispatcher.removeEventListener(FocusEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a blur event for the Tree component. A blur event fires when a component looses focus.
   *
   * @param listener The event
   * @return The component itself
   */
  public Tree addBlurListener(EventListener<BlurEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.setCallback();
    }
    dispatcher.addEventListener(BlurEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addBlurListener method.
   *
   * @see Tree #addBlurListener(EventListener)
   * @param listener A method to receive the blur event
   * @return the component itself
   */
  public Tree onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a blur event from the Tree component. Fires when a component looses focus.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Tree removeBlurListener(EventListener<BlurEvent> listener) {
    dispatcher.removeEventListener(BlurEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a mouse enter event for the Tree component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Tree addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see Tree #addMouseEnterListener(EventListener)
   * @param listener A method to receive the mouse enter event
   * @return the component itself
   */
  public Tree onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Removes a mouse enter event from the Tree component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Tree removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    dispatcher.removeEventListener(MouseEnterEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a mouse exit event for the Tree component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Tree addMouseExitListener(EventListener<MouseExitEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see Tree #addMouseEnterListener(EventListener)
   * @param listener A method to receive the mouse enter event
   * @return the component itself
   */
  public Tree onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Removes a mouse enter event from the Tree component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Tree removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    dispatcher.removeEventListener(MouseExitEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a right mouse down event for the Tree component.
   *
   * @param listener The event
   * @return The component itself
   */
  public Tree addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.setCallback();
    }
    dispatcher.addEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @see Tree #addRightMouseDownListener(EventListener)
   * @param listener A method to receive the right mouse down event
   * @return the component itself
   */
  public Tree onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Removes a right mouse down event from the Tree component.
   *
   * @param listener The event to be removed
   * @return The component itself
   */
  public Tree removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    dispatcher.removeEventListener(RightMouseDownEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.removeCallback();
    }
    return this;
  }


  /**
   * Creates the root node.
   *
   * @param id the id of the root node
   * @param text the text of the root node
   * @return the control ifself
   */
  public Tree setRoot(int id, String text) {
    this.root = TreeNode.root(id, text);
    
    if (this.bbjTree == null) {
      return this;
    }

    try {
      bbjTree.setRoot(id, text);
      return this;
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set the root", e);
    }
  }

  /**
   * Adds a note to the given parent.
   *
   * @param id the id of the new node
   * @param parentId the id of the parent node
   * @param text the text of the node
   * @return the control ifself
   */
  public Tree addNode(int id, int parentId, String text) {
    if (this.bbjTree == null) {
      final TreeNode parent = root.findNode(root, parentId);
      if (parent == null) {
        throw new DwcjRuntimeException("No node with given parentId.");
      }
      parent.addChild(parentId, text);
  
      return this;
    }

    try {
      bbjTree.addNode(id, parentId, text);
      return this;
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to add node.", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree removeAttribute(String attribute) {
    super.removeAttribute(attribute);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setId(String elementId) {
    super.setId(elementId);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setUserData(String key, Object userData) {
    super.setUserData(key, userData);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree removeStyle(String property) {
    super.removeStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree setProperty(String property, Object value) {
    super.setProperty(property, value);
    return this;
  }

  private void eventCatchUp() {
    if (this.dispatcher.getListenersCount(FocusEvent.class) > 0) {
      this.focusEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(BlurEvent.class) > 0) {
      this.blurEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(MouseEnterEvent.class) > 0) {
      this.mouseEnterEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(MouseExitEvent.class) > 0) {
      this.mouseExitEventSink.setCallback();
    }

    if (this.dispatcher.getListenersCount(RightMouseDownEvent.class) > 0) {
      this.rightMouseDownEventSink.setCallback();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();
    eventCatchUp();
  }
}
