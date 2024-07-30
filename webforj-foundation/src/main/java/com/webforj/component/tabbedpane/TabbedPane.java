package com.webforj.component.tabbedpane;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjTabCtrl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.google.gson.annotations.SerializedName;
import com.webforj.annotation.AnnotationProcessor;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.Component;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.ComponentLifecycleObserver.LifecycleEvent;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.Expanse;
import com.webforj.component.Theme;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.tabbedpane.event.TabCloseEvent;
import com.webforj.component.tabbedpane.event.TabDeselectEvent;
import com.webforj.component.tabbedpane.event.TabSelectEvent;
import com.webforj.component.tabbedpane.sink.TabCloseEventSink;
import com.webforj.component.tabbedpane.sink.TabDeselectEventSink;
import com.webforj.component.tabbedpane.sink.TabSelectEventSink;
import com.webforj.component.window.Window;
import com.webforj.concern.HasComponents;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasTheme;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * A UI component for organizing multiple components within a single container using a tabbed
 * interface.
 *
 * <p>
 * The {@code TabbedPane} class provides a compact and organized way of displaying content that is
 * divided into multiple sections, each associated with a tab. Users can switch between these
 * sections by clicking on the respective tabs, often labeled with text and/or icons. This class
 * simplifies the creation of multifaceted interfaces where different content or forms need to be
 * accessible but not simultaneously visible.
 * </p>
 *
 * <p>
 * Adding and managing tabs is straightforward using methods like {@code addTab} and
 * {@code insertTab}. Each tab is indexed in the order it's added, starting from 0 for the first
 * tab. The class maintains a selection model which manages the indices of the tabs and identifies
 * the currently active tab. The default behavior sets the first tab as the selected one if any tabs
 * are present.
 * </p>
 *
 * <p>
 * This tabbed pane is essential for applications with complex user interfaces, like configuration
 * tools, data dashboards, or applications requiring a modular approach to display information. The
 * ability to switch between different tabs without leaving the main window context makes the
 * {@code TabbedPane} an effective solution for enhancing user experience in UI design.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 *
 * @see Tab
 */
public final class TabbedPane extends DwcFocusableComponent<TabbedPane> implements Iterable<Tab>,
    HasExpanse<TabbedPane, Expanse>, HasTheme<TabbedPane, Theme>, HasComponents {

  /**
   * Describes the placement of the tabs in the tabbed pane.
   */
  public enum Placement {
    /** Place the tab in the top of the tabbed pane. */
    @SerializedName("top")
    TOP,

    /** Place the tab in the bottom of the tabbed pane. */
    @SerializedName("bottom")
    BOTTOM,

    /** Place the tab in the left of the tabbed pane. */
    @SerializedName("left")
    LEFT,

    /** Place the tab in the right of the tabbed pane. */
    @SerializedName("right")
    RIGHT,

    /** Hide the tabs from the tabbed pane. */
    @SerializedName("hidden")
    HIDDEN;
  }

  /**
   * Describes the activation mode of the tabs in the tabbed pane.
   */
  public enum Activation {
    /**
     * When set to auto, navigating tabs with the arrow keys will instantly show the corresponding
     * tab component.
     */
    @SerializedName("auto")
    AUTO,

    /**
     * When set to manual, the tab will receive focus but will not show until the user presses space
     * or enter.
     */
    @SerializedName("manual")
    MANUAL;
  }

  /**
   * Describes the removal mode of the tabs in the tabbed pane.
   */
  public enum Removal {
    /**
     * When set to auto, tabs will be removed directly when the user clicks the close button.
     */
    @SerializedName("auto")
    AUTO,

    /**
     * When set to manual, the tab is kept but the close event bubbles so it can be handled outside
     * the component.
     */
    @SerializedName("manual")
    MANUAL;
  }

  /**
   * Describes the alignment of the tabs in the tabbed pane.
   */
  public enum Alignment {
    /**
     * When set to auto, the tabs will be aligned based on the placement of the tabs.
     */
    @SerializedName("auto")
    AUTO,

    /**
     * When set to center, the tabs will be centered.
     */
    @SerializedName("center")
    CENTER,

    /**
     * When set to end, the tabs will be aligned to the end.
     */
    @SerializedName("end")
    END,

    /**
     * When set to start, the tabs will be aligned to the start.
     */
    @SerializedName("start")
    START,

    /**
     * When set to stretch, the tabs will be stretched to fill the available space.
     */
    @SerializedName("stretch")
    STRETCH;
  }

  private LinkedHashMap<Object, Tab> tabs = new LinkedHashMap<>();
  private LinkedHashMap<Object, Component> tabComponentMapping = new LinkedHashMap<>();
  private Placement placement = Placement.TOP;
  private Activation activation = Activation.AUTO;
  private Removal removal = Removal.MANUAL;
  private Alignment alignment = Alignment.AUTO;
  private boolean borderless = false;
  private boolean hideActiveIndicator = false;
  private boolean nobody = false;
  private boolean swipeable = false;
  private boolean swipeWithMouse = false;
  private String label = "";
  private int selectedIndex = 0;
  private int lastAutoGeneratedTabIndex = 0;
  private final ComponentLifecycleObserver destroyObserver =
      (Component component, LifecycleEvent event) -> {
        if (event == LifecycleEvent.DESTROY) {
          tabComponentMapping.entrySet().stream().filter(entry -> entry.getValue() != null)
              .filter(entry -> entry.getValue().equals(component))
              .forEach(entry -> entry.setValue(null));
        }
      };

  private final ComponentEventSinkRegistry<TabSelectEvent> selectEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TabSelectEventSink(this, getEventDispatcher()),
          TabSelectEvent.class);
  private final ComponentEventSinkRegistry<TabDeselectEvent> deselectEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TabDeselectEventSink(this, getEventDispatcher()),
          TabDeselectEvent.class);
  private final ComponentEventSinkRegistry<TabCloseEvent> closeEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TabCloseEventSink(this, getEventDispatcher()),
          TabCloseEvent.class);

  /**
   * Creates a new tabbed pane.
   */
  public TabbedPane() {
    super();
    setExpanse(Expanse.MEDIUM);
  }

  /**
   * Creates a new tabbed pane with the specified tabs.
   *
   * @param name The name of the tabbed pane.
   */
  public TabbedPane(String name) {
    this();
    setName(name);
  }

  /**
   * Adds a tab to the tabbed pane.
   *
   * @param tab The tab to be added.
   * @param component The component to be displayed when the tab is selected.
   *
   * @return The added tab instance.
   *
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is already in the list.
   * @throws IllegalStateException If adding a component that is destroyed.
   */
  public Tab addTab(Tab tab, Component component) {
    verifyTabDoesNotExist(tab);
    tab.addPropertyChangeListener(new TabChangeListener(this));

    if (component != null) {
      verifyValidComponentState(component);
      component.addLifecycleObserver(destroyObserver);
    }

    // start tacking
    Object key = tab.getKey();
    tabs.put(key, tab);
    tabComponentMapping.put(key, component);
    doAdd(tab, component);

    return tab;
  }

  /**
   * Adds a tab to the tabbed pane with the specified title.
   *
   * @param text The tab to be added.
   * @param component The component to be displayed when the tab is selected.
   *
   * @return The added tab instance.
   * @throws NullPointerException If the key is null.
   * @throws IllegalArgumentException If the key is already in the list.
   * @throws IllegalStateException If adding a component that is destroyed.
   */
  public Tab addTab(String text, Component component) {
    return addTab(new Tab(text), component);
  }

  /**
   * Adds a tab to the tabbed pane.
   *
   * @param tab The tab to be added.
   *
   * @return The added tab instance.
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is already in the list.
   */
  public Tab addTab(Tab tab) {
    return addTab(tab, null);
  }

  /**
   * Adds a tab to the tabbed pane with the specified title.
   *
   * @param text The tab to be added.
   *
   * @return The added tab instance.
   * @throws NullPointerException If the key is null.
   * @throws IllegalArgumentException If the key is already in the list.
   */
  public Tab addTab(String text) {
    return addTab(new Tab(text));
  }

  /**
   * Adds a component with a tab title defaulting to the name of the component which is the result
   * of calling {@code component.getName()}.
   *
   * @param components The components to be added.
   *
   * @throws NullPointerException If the component is null.
   * @throws IllegalArgumentException If the component is already in the list.
   * @throws IllegalStateException If adding a component that is destroyed.
   */
  @Override
  public void add(Component... components) {
    verifyValidComponentState(components);
    StringBuilder nameBuilder = new StringBuilder(getName());

    for (Component current : components) {
      if (lastAutoGeneratedTabIndex > 0) {
        nameBuilder.append(" (").append(lastAutoGeneratedTabIndex).append(")");
      }

      Tab tab = new Tab(nameBuilder.toString());
      lastAutoGeneratedTabIndex++;
      // add tab and component
      addTab(tab, current);
    }
  }

  /**
   * Insert a tab in the tabbed pane at the specified index.
   *
   * @param index The index at which the tab should be inserted.
   * @param tab The tab to be added.
   * @param component The component to be displayed when the tab is selected.
   *
   * @return The added tab instance.
   *
   * @throws NullPointerException If the tab is null
   * @throws IllegalArgumentException If the tab is already in the list.
   * @throws IllegalStateException If adding a component that is destroyed.
   */
  public Tab insertTab(int index, Tab tab, Component component) {
    verifyValidIndex(index, true);

    verifyTabDoesNotExist(tab);
    tab.addPropertyChangeListener(new TabChangeListener(this));

    if (component != null) {
      verifyValidComponentState(component);
      component.addLifecycleObserver(destroyObserver);
    }

    // start tacking
    tabs = insertInMapAt(tabs, index, tab.getKey(), tab);
    tabComponentMapping = insertInMapAt(tabComponentMapping, index, tab.getKey(), null);
    doInsert(index, tab, component);

    return tab;
  }

  /**
   * Insert a tab in the tabbed pane at the specified index.
   *
   * @param index The index at which the tab should be inserted.
   * @param text The tab to be added.
   * @param component The component to be displayed when the tab is selected.
   *
   * @return The added tab instance.
   * @throws NullPointerException If the key is null.
   * @throws IllegalArgumentException If the key is already in the list.
   * @throws IllegalStateException If adding a component that is destroyed.
   */
  public Tab insertTab(int index, String text, Component component) {
    return insertTab(index, new Tab(text), component);
  }

  /**
   * Insert a tab in the tabbed pane at the specified index.
   *
   * @param index The index at which the tab should be inserted.
   * @param tab The tab to be added.
   *
   * @return The added tab instance.
   *
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is already in the list.
   */
  public Tab insertTab(int index, Tab tab) {
    return insertTab(index, tab, null);
  }

  /**
   * Insert a tab in the tabbed pane at the specified index.
   *
   * @param index The index at which the tab should be inserted.
   * @param text The tab to be added.
   *
   * @return The added tab instance.
   * @throws NullPointerException If the key is null.
   * @throws IllegalArgumentException If the key is already in the list.
   */
  public Tab insertTab(int index, String text) {
    return insertTab(index, new Tab(text));
  }

  /**
   * Set the component for the specified tab.
   *
   * <p>
   * If the tab is already associated with a component, the old component will be destroyed.
   * </p>
   *
   * @param tab The tab to set the component for.
   * @param component The component to set.
   *
   * @return the component itself.
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is not in the tabbed pane.
   * @throws IllegalStateException If the component is destroyed.
   */
  public TabbedPane setComponentFor(Tab tab, Component component) {
    verifyTabExists(tab);
    verifyValidComponentState(component);

    // destroy old component if exists
    Component oldComponent = tabComponentMapping.get(tab.getKey());
    if (oldComponent != null) {
      oldComponent.destroy();
    }

    // add new component
    tabComponentMapping.put(tab.getKey(), component);

    BBjTabCtrl tabCtrl = inferTabCtrl();

    if (tabCtrl != null) {
      try {
        int index = indexOf(tab);
        tabCtrl.setControlAt(index, inferComponentControl(component));
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Set the component for the specified tab index.
   *
   * <p>
   * If the tab is already associated with a component, the old component will be destroyed.
   * </p>
   *
   * @param index The tab index to set the component for.
   * @param component The component to set.
   *
   * @return the component itself.
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is not in the tabbed pane.
   * @throws IllegalStateException If the component is destroyed.
   */
  public TabbedPane setComponentFor(int index, Component component) {
    verifyValidIndex(index, false);
    return setComponentFor(getTab(index), component);
  }

  /**
   * Gets the component for the specified tab.
   *
   * @param tab The tab to set the component for.
   *
   * @return the associated component for the specified tab
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is not in the tabbed pane.
   */
  public Component getComponentFor(Tab tab) {
    verifyTabExists(tab);
    return tabComponentMapping.get(tab.getKey());
  }

  /**
   * Gets the component for the specified tab index.
   *
   * @param index The tab index to set the component for.
   *
   * @return the associated component for the specified tab index
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is not in the tabbed pane.
   */
  public Component getComponentFor(int index) {
    verifyValidIndex(index, false);
    return getComponentFor(getTab(index));
  }

  /**
   * Gets the tab for the specified component.
   *
   * @param component The component to get the tab for.
   *
   * @return the associated tab for the specified component
   * @throws NullPointerException If the component is null.
   * @throws IllegalArgumentException If the component is not in the tabbed pane.
   */
  public Tab getTabFor(Component component) {
    for (Map.Entry<Object, Component> entry : tabComponentMapping.entrySet()) {
      if (entry.getValue().equals(component)) {
        return tabs.get(entry.getKey());
      }
    }

    return null;
  }

  /**
   * Removes the specified tab from the tabbed pane.
   *
   * <p>
   * Removing a tab will remove also its associated component.
   * </p>
   *
   * @param tab The tab to be removed.
   *
   * @return The component itself.
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is not in the tabbed pane.
   */
  public TabbedPane removeTab(Tab tab) {
    verifyTabExists(tab);
    BBjTabCtrl tabCtrl = inferTabCtrl();

    if (tabCtrl != null) {
      try {
        int index = indexOf(tab);
        Component component = getComponentFor(index);
        if (component != null) {
          remove(component);
        }

        tabCtrl.removeTab(index);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    tabs.remove(tab.getKey());
    tabComponentMapping.remove(tab.getKey());

    return this;
  }

  /**
   * Removes the tab at the specified index from the tabbed pane.
   *
   * @param index The index of the tab to be removed.
   *
   * @return The component itself.
   * @throws IndexOutOfBoundsException If the index is out of range.
   */
  public TabbedPane removeTab(int index) {
    verifyValidIndex(index, false);
    return removeTab(getTab(index));
  }

  /**
   * Removes all tabs from the tabbed pane.
   *
   * @return The component itself.
   */
  public TabbedPane removeAllTabs() {
    Collection<Tab> tabValues = new ArrayList<>(tabs.values());
    for (Tab tab : tabValues) {
      removeTab(tab);
    }

    // reset
    tabs.clear();
    tabComponentMapping.clear();

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove(Component... components) {
    if (components == null) {
      throw new NullPointerException("Cannot remove a null component");
    }

    for (Component current : components) {
      if (current == null) {
        throw new NullPointerException("Cannot remove a null component");
      }

      if (tabComponentMapping.containsValue(current)) {
        tabComponentMapping.entrySet().stream().filter(entry -> entry.getValue() != null)
            .filter(entry -> entry.getValue().equals(current))
            .forEach(entry -> entry.setValue(null));
        current.destroy();
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAll() {
    getComponents().forEach(Component::destroy);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Component> getComponents() {
    return Collections.unmodifiableList(
        new ArrayList<>(tabComponentMapping.values().stream().filter(Objects::nonNull).toList()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getComponent(String id) {
    List<Component> components = getComponents();
    for (Component component : components) {
      if (component.getComponentId().equals(id)) {
        return component;
      }
    }

    return null;
  }

  /**
   * Returns the index of the given tab.
   *
   * @param tab The tab to search for.
   * @return The index of the tab, or -1 if the key is not found.
   */
  public int indexOf(Tab tab) {
    int index = 0;
    for (Map.Entry<Object, Tab> entry : tabs.entrySet()) {
      if (entry.getKey().equals(tab.getKey())) {
        return index;
      }

      index++;
    }

    return -1;
  }

  /**
   * Returns the tab at the specified index.
   *
   * @param index The index of the tab to retrieve.
   * @return The tab, or null if the index is out of range.
   */
  public Tab getTab(int index) {
    verifyValidIndex(index, false);
    return (Tab) tabs.values().toArray()[index];
  }

  /**
   * Returns unmodifiable list of tabs.
   *
   * @return The list of tabs.
   */
  public List<Tab> getTabs() {
    return Collections.unmodifiableList(new ArrayList<>(tabs.values()));
  }

  /**
   * Returns whether the list contains the specified tab.
   *
   * @param tab The tab to search for
   * @return True if the tab is found, false otherwise.
   */
  public boolean hasTab(Tab tab) {
    return tabs.containsValue(tab);
  }

  /**
   * Returns whether the tabbed pane is empty.
   *
   * @return True if the tab is empty
   */
  public boolean isEmpty() {
    return tabs.isEmpty();
  }

  /**
   * Returns the total count of tabs in the the tabbed pane.
   *
   * @return The number of tabs.
   */
  public int size() {
    return tabs.size();
  }

  /**
   * Returns an iterator over the entries in the tabbed pane.
   *
   * @return An iterator over the tab entries.
   */
  public Iterator<Tab> iterator() {
    return Collections.unmodifiableCollection(tabs.values()).iterator();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public TabbedPane setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Expanse getExpanse() {
    return getComponentExpanse();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public TabbedPane setTheme(Theme theme) {
    setComponentTheme(theme);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Theme getTheme() {
    return super.<Theme>getComponentTheme();
  }

  /**
   * Sets the placement of the tabs in the tabbed pane.
   *
   * @param placement The placement to set.
   *
   * @return The component itself.
   */
  public TabbedPane setPlacement(Placement placement) {
    this.placement = placement;
    setUnrestrictedProperty("placement", this.placement);
    return this;
  }

  /**
   * Returns the placement of the tabs in the tabbed pane.
   *
   * @return The placement of the tabs.
   */
  public Placement getPlacement() {
    return placement;
  }

  /**
   * Sets the activation mode of the tabs in the tabbed pane.
   *
   * @param activation The activation mode to set.
   *
   * @return The component itself.
   */
  public TabbedPane setActivation(Activation activation) {
    this.activation = activation;
    setUnrestrictedProperty("activation", this.activation);
    return this;
  }

  /**
   * Returns the activation mode of the tabs in the tabbed pane.
   *
   * @return The activation mode of the tabs.
   */
  public Activation getActivation() {
    return activation;
  }

  /**
   * Sets the removal mode of the tabs in the tabbed pane.
   *
   * @param removal The removal mode to set.
   *
   * @return The component itself.
   */
  public TabbedPane setRemoval(Removal removal) {
    this.removal = removal;
    setUnrestrictedProperty("removal", this.removal);
    return this;
  }

  /**
   * Returns the removal mode of the tabs in the tabbed pane.
   *
   * @return The removal mode of the tabs.
   */
  public Removal getRemoval() {
    return removal;
  }

  /**
   * Sets the alignment of the tabs in the tabbed pane.
   *
   * @param alignment The alignment to set.
   *
   * @return The component itself.
   */
  public TabbedPane setAlignment(Alignment alignment) {
    this.alignment = alignment;
    setUnrestrictedProperty("alignment", this.alignment);
    return this;
  }

  /**
   * Returns the alignment of the tabs in the tabbed pane.
   *
   * @return The alignment of the tabs.
   */
  public Alignment getAlignment() {
    return alignment;
  }

  /**
   * Sets whether the tabbed pane should be borderless.
   *
   * @param borderless {@code true} to make the tabbed pane borderless.
   *
   * @return The component itself.
   */
  public TabbedPane setBorderless(boolean borderless) {
    this.borderless = borderless;
    setUnrestrictedProperty("borderless", this.borderless);
    return this;
  }

  /**
   * Returns whether the tabbed pane is borderless.
   *
   * @return {@code true} if the tabbed pane is borderless.
   */
  public boolean isBorderless() {
    return borderless;
  }

  /**
   * Sets whether the active indicator should be hidden.
   *
   * @param hideActiveIndicator {@code true} to hide the active indicator.
   *
   * @return The component itself.
   */
  public TabbedPane setHideActiveIndicator(boolean hideActiveIndicator) {
    this.hideActiveIndicator = hideActiveIndicator;
    setUnrestrictedProperty("hideActiveIndicator", this.hideActiveIndicator);
    return this;
  }

  /**
   * Returns whether the active indicator is hidden.
   *
   * @return {@code true} if the active indicator is hidden.
   */
  public boolean isHideActiveIndicator() {
    return hideActiveIndicator;
  }

  /**
   * Sets whether the body should be hidden.
   *
   * <p>
   * Useful when the tabbed pane is being used as a menu and tabs are not associated with any
   * component.
   * </p>
   *
   * @param nobody {@code true} to hide the body.
   *
   * @return The component itself.
   *
   * @deprecated Use {@link #setBodyHidden(boolean)} instead.
   */
  @Deprecated(since = "24.00", forRemoval = true)
  public TabbedPane hideBody(boolean nobody) {
    return setBodyHidden(nobody);
  }

  /**
   * Sets whether the body should be hidden.
   *
   * <p>
   * Useful when the tabbed pane is being used as a menu and tabs are not associated with any
   * component.
   * </p>
   *
   * @param nobody {@code true} to hide the body.
   *
   * @return The component itself.
   */
  public TabbedPane setBodyHidden(boolean nobody) {
    this.nobody = nobody;
    setUnrestrictedProperty("nobody", this.nobody);
    return this;
  }

  /**
   * Returns whether the body is hidden.
   *
   * @return {@code true} if the body is hidden.
   */
  public boolean isBodyHidden() {
    return nobody;
  }

  /**
   * Sets the label of the tabbed pane.
   *
   * <p>
   * The label is used for accessibility purposes. It not visible when the tabbed pane is rendered.
   * </p>
   *
   * @param label The label to set.
   *
   * @return The component itself.
   */
  public TabbedPane setLabel(String label) {
    this.label = label;
    setUnrestrictedProperty("label", this.label);
    return this;
  }

  /**
   * Returns the label of the tabbed pane.
   *
   * @return The label of the tabbed pane.
   */
  public String getLabel() {
    return label;
  }

  /**
   * Sets whether the tabs should be swipeable.
   *
   * @param swipeable When {@code true}, active tab can be changed by swiping the tabbed pane.
   *
   * @return The component itself.
   */
  public TabbedPane setSwipeable(boolean swipeable) {
    this.swipeable = swipeable;
    setUnrestrictedProperty("swipeable", this.swipeable);
    return this;
  }

  /**
   * Returns whether the tabs are swipeable.
   *
   * @return {@code true} if the tabs are swipeable.
   */
  public boolean isSwipeable() {
    return swipeable;
  }

  /**
   * Sets whether the tabs should be swipeable with the mouse.
   *
   * @param swipeWithMouse When {@code true}, active tab can be changed by swiping the tabbed pane
   *        with the mouse.
   *
   * @return The component itself.
   */
  public TabbedPane setSwipeWithMouse(boolean swipeWithMouse) {
    this.swipeWithMouse = swipeWithMouse;
    setUnrestrictedProperty("swipeWithMouse", this.swipeWithMouse);
    return this;
  }

  /**
   * Returns whether the tabs are swipeable with the mouse.
   *
   * @return {@code true} if the tabs are swipeable with the mouse.
   */
  public boolean isSwipeWithMouse() {
    return swipeWithMouse;
  }

  /**
   * Selects the specified tab.
   *
   * @param tab The tab to select.
   *
   * @return The component itself.
   * @throws NullPointerException If the tab is null.
   * @throws IllegalArgumentException If the tab is not in the tabbed pane.
   */
  public TabbedPane select(Tab tab) {
    verifyTabExists(tab);
    int index = indexOf(tab);
    BBjTabCtrl tabCtrl = inferTabCtrl();

    if (tabCtrl != null) {
      try {
        tabCtrl.setSelectedIndex(index);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.selectedIndex = index;

    return this;
  }

  /**
   * Selects the tab at the specified index.
   *
   * @param index The index of the tab to select.
   *
   * @return The component itself.
   *
   * @throws IndexOutOfBoundsException If the index is out of range.
   */
  public TabbedPane select(int index) {
    verifyValidIndex(index, false);
    return select(getTab(index));
  }

  /**
   * Gets the selected tab.
   *
   * @return The selected tab or null if no tab is selected
   */
  public Tab getSelected() {
    BBjTabCtrl tabCtrl = inferTabCtrl();

    if (tabCtrl != null) {
      try {
        return getTab(tabCtrl.getSelectedIndex());
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    } else {
      return getTab(selectedIndex);
    }
  }

  /**
   * Gets the index of the selected tab.
   *
   * @return The index of the selected tab or -1 if no tab is selected
   */
  public int getSelectedIndex() {
    Tab tab = getSelected();

    if (tab != null) {
      return indexOf(tab);
    }

    return -1;
  }

  /**
   * Adds a {@link TabSelectEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TabSelectEvent> addSelectListener(
      EventListener<TabSelectEvent> listener) {
    return selectEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Adds a {@link TabSelectEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TabSelectEvent> onSelect(EventListener<TabSelectEvent> listener) {
    return addSelectListener(listener);
  }

  /**
   * Adds a {@link TabDeselectEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TabDeselectEvent> addDeselectListener(
      EventListener<TabDeselectEvent> listener) {
    return deselectEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Adds a {@link TabDeselectEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TabDeselectEvent> onDeselect(
      EventListener<TabDeselectEvent> listener) {
    return addDeselectListener(listener);
  }

  /**
   * Adds a {@link TabCloseEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TabCloseEvent> addCloseListener(
      EventListener<TabCloseEvent> listener) {
    return closeEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Adds a {@link TabCloseEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TabCloseEvent> onClose(EventListener<TabCloseEvent> listener) {
    return addCloseListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("activation", "alignment", "borderless", "disabled", "expanse",
        "hideActiveIndicator", "label", "nobody", "placement", "removal", "selected",
        "swipePreventScroll", "swipeWithMouse", "swipeWithTouch", "swipeable", "swiping", "theme"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addTabCtrl(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create the BBjTabCtrl Control", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    selectEventSinkListenerRegistry.attach();
    deselectEventSinkListenerRegistry.attach();
    closeEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    // loop over tab component mappings and add the components
    for (Map.Entry<Object, Component> entry : tabComponentMapping.entrySet()) {
      Tab tab = tabs.get(entry.getKey());
      Component component = entry.getValue();
      doAdd(tab, component);

      int index = indexOf(tab);
      BBjTabCtrl tabCtrl = inferTabCtrl();

      try {
        if (!tab.isEnabled()) {
          tabCtrl.setEnabledAt(index, false);
        }

        if (tab.isClosable()) {
          tabCtrl.setCloseableAt(index, true);
        }

        if (tab.getTooltip() != null && !tab.getTooltip().isBlank()) {
          tabCtrl.setToolTipTextAt(index, tab.getTooltip());
        }
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    // re-apply selected tab if any
    if (selectedIndex != 0) {
      select(selectedIndex);
    }
  }

  private void doAdd(Tab tab, Component component) {
    BBjTabCtrl tabCtrl = inferTabCtrl();

    if (tabCtrl != null) {
      try {
        if (component != null) {
          tabCtrl.addTab(tab.getText(), inferComponentControl(component));
        } else {
          tabCtrl.addTab(tab.getText(), -1);
        }
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
  }

  private void doInsert(int index, Tab tab, Component component) {
    BBjTabCtrl tabCtrl = inferTabCtrl();

    if (tabCtrl != null) {
      try {
        if (component != null) {
          tabCtrl.insertTab(index, tab.getText(), inferComponentControl(component));
        } else {
          tabCtrl.insertTab(index, tab.getText(), -1);
        }
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
  }

  private void verifyValidIndex(int index, boolean insert) {
    if (index < 0 || index >= tabs.size() + (insert ? 1 : 0)) {
      throw new IndexOutOfBoundsException(String.format("Invalid Tab index '%d'", index));
    }
  }

  private void verifyTabDoesNotExist(Tab tab) {
    if (tab == null) {
      throw new NullPointerException("Tab cannot be null");
    }

    if (hasTab(tab)) {
      throw new IllegalArgumentException("Tab already exists");
    }
  }

  protected void verifyTabExists(Tab tab) {
    if (tab == null) {
      throw new NullPointerException("Tab cannot be null");
    }

    if (!hasTab(tab)) {
      throw new IllegalArgumentException("Tab does not exist");
    }
  }

  private void verifyValidComponentState(Component... components) {
    if (components == null) {
      throw new NullPointerException("Cannot add a null component");
    }

    for (Component current : components) {
      if (current == null) {
        throw new NullPointerException("Cannot add a null component");
      }

      // adding a component that is destroyed should throw an exception
      if (current.isDestroyed()) {
        throw new IllegalStateException("Cannot add a component that is destroyed");
      }

      // adding a component with the same id should throw an exception
      if (tabComponentMapping.containsValue(current)) {
        throw new IllegalArgumentException(
            "Component with id '" + current.getClass().getName() + "' already exists");
      }
    }
  }

  private BBjControl inferComponentControl(Component component) {
    try {
      ComponentAccessor.getDefault().create(component, getWindow());
      AnnotationProcessor processor = new AnnotationProcessor();
      processor.processControlAnnotations(component);
      return ComponentAccessor.getDefault().getControl(component);

    } catch (IllegalAccessException e) {
      throw new IllegalArgumentException("Attempted to add an incompatible component of type '"
          + component.getClass().getName() + "'. "
          + "Components must be a subclass of DwcComponent or extend Composite. "
          + "Check if the component is correctly extending or implementing these " + "classes.");
    }
  }

  BBjTabCtrl inferTabCtrl() {
    try {
      return (BBjTabCtrl) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  private static <K, V> LinkedHashMap<K, V> insertInMapAt(LinkedHashMap<K, V> map, int index, K key,
      V value) {
    int i = 0;
    LinkedHashMap<K, V> newMap = new LinkedHashMap<>();

    for (Map.Entry<K, V> entry : map.entrySet()) {
      if (i == index) {
        newMap.put(key, value);
      }

      newMap.put(entry.getKey(), entry.getValue());
      i++;
    }

    if (i <= index) {
      newMap.put(key, value);
    }

    return newMap;
  }
}
