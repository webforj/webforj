package org.dwcj.component.tabbedpane;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.UUID;

/**
 * Represents a tab used in tabbed pane components.
 *
 * <p>
 * Tabs are UI elements that can be added to tabbed panes to organize and switch between different
 * content views.
 * </p>
 *
 * <p>
 * <strong>Note:</strong> Tabs are not intended to be used as standalone components. They are
 * intended to be used in conjunction with tabbed panes. The class itself is not a {@code Component}
 * and should not be used as such.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 *
 * @see TabbedPane
 */
public final class Tab {
  private Object key = null;
  private String text = "";
  private String tooltip = "";
  private boolean enabled = true;
  private boolean closable = false;
  private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);

  /**
   * Constructs a tab with the given text.
   *
   * @param text the tab text.
   */
  public Tab(String text) {
    this.key = UUID.randomUUID();
    this.text = text;
  }

  /**
   * Gets the key of the tab.
   *
   * @return the key
   */
  Object getKey() {
    return key;
  }

  /**
   * Gets the text of the tab.
   *
   * @return the text
   */
  public String getText() {
    return text;
  }

  /**
   * Sets the text of the tab.
   *
   * @param text the text to set
   * @return the tab
   */
  public Tab setText(String text) {
    String oldValue = this.text;
    this.text = text;
    this.changeSupport.firePropertyChange("text", oldValue, text);

    return this;
  }

  /**
   * Alias for {@link #getText()}.
   *
   * @return the title
   */
  public String getTitle() {
    return getText();
  }

  /**
   * Alias for {@link #setText(String)}.
   *
   * @param title the title to set
   * @return the tab
   */
  public Tab setTitle(String title) {
    return setText(title);
  }

  /**
   * Gets the tooltip of the tab.
   *
   * @return the tooltip
   */
  public String getTooltip() {
    return tooltip;
  }

  /**
   * Sets the tooltip of the tab.
   *
   * @param tooltip the tooltip to set
   * @return the tab
   */
  public Tab setTooltip(String tooltip) {
    String oldValue = this.tooltip;
    this.tooltip = tooltip;
    this.changeSupport.firePropertyChange("tooltip", oldValue, tooltip);
    return this;
  }

  /**
   * Gets whether the tab is enabled.
   *
   * @return true if the tab is enabled, false otherwise
   */
  public boolean isEnabled() {
    return enabled;
  }

  /**
   * Sets whether the tab is enabled.
   *
   * @param enabled true if the tab is enabled, false otherwise
   * @return the tab
   */
  public Tab setEnabled(boolean enabled) {
    boolean oldValue = this.enabled;
    this.enabled = enabled;
    this.changeSupport.firePropertyChange("enabled", oldValue, enabled);

    return this;
  }

  /**
   * Gets whether the tab is closable.
   *
   * @return true if the tab is closable, false otherwise
   */
  public boolean isClosable() {
    return closable;
  }

  /**
   * Sets whether the tab is closable.
   *
   * @param closable true if the tab is closable, false otherwise
   * @return the tab
   */
  public Tab setClosable(boolean closable) {
    boolean oldValue = this.closable;
    this.closable = closable;
    this.changeSupport.firePropertyChange("closable", oldValue, closable);

    return this;
  }

  /**
   * Adds a property change listener.
   *
   * @param listener the listener
   * @return the list item
   */
  Tab addPropertyChangeListener(PropertyChangeListener listener) {
    this.changeSupport.addPropertyChangeListener(listener);
    return this;
  }
}
