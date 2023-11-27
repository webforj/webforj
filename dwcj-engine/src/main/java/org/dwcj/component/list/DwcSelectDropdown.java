package org.dwcj.component.list;

import com.basis.bbj.proxies.sysgui.BBjComboBox;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.event.EventSinkListenerRegistry;
import org.dwcj.component.list.event.ListClickEvent;
import org.dwcj.component.list.event.ListCloseEvent;
import org.dwcj.component.list.event.ListOpenEvent;
import org.dwcj.component.list.event.ListSelectEvent;
import org.dwcj.component.list.sink.ListClickEventSink;
import org.dwcj.component.list.sink.ListCloseEventSink;
import org.dwcj.component.list.sink.ListOpenEventSink;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * An abstract class representing a selectable dropdown component that extends the functionality of
 * a list.
 *
 * @param <T> The type of component that this dropdown extends.
 *
 * @see ChoiceBox
 * @see ComboBox
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class DwcSelectDropdown<T extends DwcList<T>> extends DwcList<T> {
  private final EventSinkListenerRegistry<ListOpenEvent> openEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ListOpenEventSink(this, getEventDispatcher()),
          ListOpenEvent.class);
  private final EventSinkListenerRegistry<ListCloseEvent> closeEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ListCloseEventSink(this, getEventDispatcher()),
          ListCloseEvent.class);
  private final EventSinkListenerRegistry<ListClickEvent> clickEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ListClickEventSink(this, getEventDispatcher()),
          ListClickEvent.class);

  private String openWidth = "";
  private String openHeight = "";
  private boolean opened = false;
  private int maxRowCount = 0;
  private String dropdownType = "";

  /**
   * Constructs a new DwcSelectDropdown.
   */
  protected DwcSelectDropdown() {
    super();
  }

  /**
   * Constructs a new DwcSelectDropdown.
   *
   * @param label the label of the component
   */
  protected DwcSelectDropdown(String label) {
    super(label);
  }

  /**
   * Constructs a new DwcSelectDropdown.
   *
   * @param label the label of the component
   * @param selectListener the listener to be called when the user selects an item
   */
  protected DwcSelectDropdown(String label, EventListener<ListSelectEvent> selectListener) {
    super(label, selectListener);
  }

  /**
   * Open the component's list.
   *
   * @return the component itself.
   */
  public T open() {
    this.opened = true;

    BBjComboBox bbjControl = inferComboBox();

    if (bbjControl != null) {
      try {
        bbjControl.openList();
      } catch (Exception e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Returns true if the component's list is opened, false otherwise.
   *
   * @return true if the component's list is opened, false otherwise.
   */
  public boolean isOpened() {
    Object isOpened = getProperty("opened");
    return isOpened == null ? this.opened : Boolean.valueOf(String.valueOf(isOpened));
  }

  /**
   * Close the component's list.
   *
   * @return the component itself.
   */
  public T close() {
    this.opened = false;

    BBjComboBox bbjControl = inferComboBox();

    if (bbjControl != null) {
      try {
        bbjControl.closeList();
      } catch (Exception e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Sets the dropdown min width.
   *
   * @param width the minimum width of the component's list.
   * @return the component itself.
   */
  public T setOpenWidth(String width) {
    this.openWidth = width;
    setUnrestrictedProperty("openWidth", width);
    return getSelf();
  }

  /**
   * Sets the dropdown min width in pixels.
   *
   * @param width the minimum width of the component's list.
   * @return the component itself.
   *
   * @see #setOpenWidth(String)
   */
  public T setOpenWidth(int width) {
    return setOpenWidth(width + "px");
  }

  /**
   * Returns the minimum width of the component's list.
   *
   * @return the minimum width of the component's list.
   */
  public String getOpenWidth() {
    return this.openWidth;
  }

  /**
   * Sets the dropdown max height.
   *
   * @param height the maximum height of the component's list.
   * @return the component itself.
   */

  public T setOpenHeight(String height) {
    this.openHeight = height;
    setUnrestrictedProperty("openHeight", height);
    return getSelf();
  }

  /**
   * Sets the dropdown max height in pixels.
   *
   * @param dropdownMaxHeight the maximum height of the component's list.
   * @return the component itself.
   *
   * @see #setOpenHeight(String)
   */
  public T setOpenHeight(int dropdownMaxHeight) {
    return setOpenHeight(dropdownMaxHeight + "px");
  }

  /**
   * Returns the maximum height of the component's list.
   *
   * @return the maximum height of the component's list.
   */
  public String getOpenHeight() {
    return this.openHeight;
  }

  /**
   * Sets the maximum number of rows that the drop-down list will display.
   *
   * @param maxRowCount the maxRowCount to set. The default value is 0. which means that the number
   *        of rows will be increased to fit the content.
   * @return the component itself.
   */
  public T setMaxRowCount(int maxRowCount) {
    this.maxRowCount = maxRowCount;
    setUnrestrictedProperty("maxRowCount", maxRowCount);
    return getSelf();
  }

  /**
   * Returns the maximum number of rows that the drop-down list will display.
   *
   * @return the maximum number
   */
  public int getMaxRowCount() {
    return this.maxRowCount;
  }

  /**
   * Gives the dropdown a custom type.
   *
   * <p>
   * You have the option to assign a custom type to the dropdown, and this custom type will appear
   * in the dropdown list as a DOM attribute named <code>data-dropdown-for="TYPE"</code>. This
   * attribute can be quite valuable for styling purposes.
   * </p>
   *
   * <p>
   * When you open the dropdown, it's taken out of its current position in the DOM and relocated to
   * the end of the page body. This detachment creates a situation where directly targeting the
   * dropdown using CSS or shadow part selectors from the parent component becomes challenging,
   * unless you make use of the dropdown type attribute.
   * </p>
   *
   *
   * @param type a custom dropdown type.
   * @return the component itself.
   */
  public T setDropdownType(String type) {
    this.dropdownType = type;
    setUnrestrictedProperty("type", type);
    return getSelf();
  }

  /**
   * Returns the dropdown type.
   *
   * @return the dropdown type.
   */
  public String getDropdownType() {
    return this.dropdownType;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T deselect() {
    getInternalSelectedList().clear();

    BBjComboBox list = inferComboBox();

    if (list != null) {
      try {
        list.deselect();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Adds a {@link ListOpenEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ListOpenEvent> addOpenListener(
      EventListener<ListOpenEvent> listener) {
    return openEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Adds a {@link ListOpenEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ListOpenEvent> onOpen(EventListener<ListOpenEvent> listener) {
    return addOpenListener(listener);
  }

  /**
   * Adds a {@link ListCloseEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ListCloseEvent> addCloseListener(
      EventListener<ListCloseEvent> listener) {
    return closeEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Adds a {@link ListCloseEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ListCloseEvent> onClose(EventListener<ListCloseEvent> listener) {
    return addCloseListener(listener);
  }

  /**
   * Adds a {@link ListClickListener} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ListClickEvent> addClickListener(
      EventListener<ListClickEvent> listener) {
    return clickEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Adds a {@link ListClickEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ListClickEvent> onClick(EventListener<ListClickEvent> listener) {
    return addClickListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    openEventSinkListenerRegistry.attach();
    closeEventSinkListenerRegistry.attach();
    clickEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    catchupItemsChanges();
    catchupSingleSelection();

    if (this.opened) {
      open();
    }
  }

  private BBjComboBox inferComboBox() {
    try {
      return (BBjComboBox) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }
}
