package com.webforj.component.tabbedpane;

import com.webforj.component.Component;
import com.webforj.component.SlotRegistry;
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
  static final String PREFIX_SLOT = "prefix";
  static final String SUFFIX_SLOT = "suffix";
  private Object key = null;
  private String text = "";
  private String tooltip = "";
  private boolean enabled = true;
  private boolean closable = false;
  private Component prefix = null;
  private Component suffix = null;
  private final SlotRegistry slotRegistry = new SlotRegistry();
  private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);

  /**
   * Constructs a tab with the given text and prefix component.
   *
   * @param text the tab text.
   * @param prefix the prefix component
   * @since 24.11
   */
  public Tab(String text, Component prefix) {
    this.key = UUID.randomUUID();
    setText(text);
    if (prefix != null) {
      setPrefixComponent(prefix);
    }
  }

  /**
   * Constructs a tab with the given text.
   *
   * @param text the tab text.
   */
  public Tab(String text) {
    this(text, null);
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
   * Sets the prefix component for the tab.
   *
   * <p>
   * The prefix component is the component that is displayed in the tab's prefix slot. If a prefix
   * component is already set, then the old prefix component will be destroyed and replaced with the
   * new one.
   * </p>
   *
   * @param prefix the prefix component to set
   * @return the tab
   * @since 24.11
   */
  public Tab setPrefixComponent(Component prefix) {
    Component oldValue = this.prefix;
    this.prefix = prefix;
    this.changeSupport.firePropertyChange(PREFIX_SLOT, oldValue, prefix);
    slotRegistry.replaceComponentsInSlot(PREFIX_SLOT, prefix);

    return this;
  }

  /**
   * Retrieves the prefix component for the tab.
   *
   * @return the prefix component for the tab
   * @since 24.11
   */
  public Component getPrefixComponent() {
    return prefix;
  }

  /**
   * Sets the suffix component for the tab.
   *
   * <p>
   * The suffix component is the component that is displayed in the tab's suffix slot. If a suffix
   * component is already set, then the old suffix component will be destroyed and replaced with the
   * new one.
   * </p>
   *
   * @param suffix the suffix component to set
   * @return the tab
   * @since 24.11
   */
  public Tab setSuffixComponent(Component suffix) {
    Component oldValue = this.suffix;
    this.suffix = suffix;
    this.changeSupport.firePropertyChange(SUFFIX_SLOT, oldValue, suffix);
    slotRegistry.addComponentsToSlot(SUFFIX_SLOT, suffix);

    return this;
  }

  /**
   * Retrieves the suffix component for the tab.
   *
   * @return the suffix component for the tab
   * @since 24.11
   */
  public Component getSuffixComponent() {
    return suffix;
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

  /**
   * Gets the slot registry associated with this tab.
   *
   * @return the slot registry
   */
  SlotRegistry getSlotRegistry() {
    return slotRegistry;
  }
}
