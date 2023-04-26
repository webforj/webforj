package org.dwcj.component.tree;

import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.util.BBjFunctionalityHelper;

/**
 * A Tree control.
 */
public final class Tree extends AbstractDwcComponent {

  private BBjTree bbjTree;

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags = 
        BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addTree(flags);
      bbjTree = (BBjTree) ctrl;
      catchUp();
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create Tree", e);
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

  /**
   * {@inheritDoc}
   */
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();

  }
}
