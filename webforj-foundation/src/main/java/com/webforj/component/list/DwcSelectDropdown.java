package com.webforj.component.list;

import com.basis.bbj.proxies.sysgui.BBjComboBox;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.list.event.ListClickEvent;
import com.webforj.component.list.event.ListCloseEvent;
import com.webforj.component.list.event.ListOpenEvent;
import com.webforj.component.list.event.ListSelectEvent;
import com.webforj.component.list.sink.ListClickEventSink;
import com.webforj.component.list.sink.ListCloseEventSink;
import com.webforj.component.list.sink.ListOpenEventSink;
import com.webforj.data.binding.Binding;
import com.webforj.data.binding.BindingContext;
import com.webforj.data.binding.concern.BindAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;

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
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public abstract class DwcSelectDropdown<T extends DwcList<T, Object>> extends DwcList<T, Object>
    implements BindAware {
  private final ComponentEventSinkRegistry<ListOpenEvent> openEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new ListOpenEventSink(this, getEventDispatcher()),
          ListOpenEvent.class);
  private final ComponentEventSinkRegistry<ListCloseEvent> closeEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new ListCloseEventSink(this, getEventDispatcher()),
          ListCloseEvent.class);
  private final ComponentEventSinkRegistry<ListClickEvent> clickEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new ListClickEventSink(this, getEventDispatcher()),
          ListClickEvent.class);

  private String openWidth = "";
  private String openHeight = "";
  private boolean opened = false;
  private int maxRowCount = 0;
  private String dropdownType = "";
  private boolean registeredSelectValueChangeListener = false;

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
  protected DwcSelectDropdown(String label, EventListener<ListSelectEvent<Object>> selectListener) {
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
        throw new WebforjRuntimeException(e);
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
        throw new WebforjRuntimeException(e);
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
        throw new WebforjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Alias for {@link #selectKey(Object)}.
   *
   * @param key the key of the item to be selected.
   *
   * @return the component itself.
   */
  @Override
  public T setValue(Object key) {
    try {
      return selectKey(key);
    } catch (IllegalArgumentException e) {
      // pass
    }

    return getSelf();
  }

  /**
   * Alias for {@link #getSelected()}.
   *
   * @return the selected key.
   */
  @Override
  public Object getValue() {
    return getSelectedKey();
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
  public ListenerRegistration<ValueChangeEvent<Object>> addValueChangeListener(
      EventListener<ValueChangeEvent<Object>> listener) {
    ListenerRegistration<ValueChangeEvent<Object>> registration =
        getEventDispatcher().addListener(ValueChangeEvent.class, listener);

    if (!registeredSelectValueChangeListener) {
      addSelectListener(ev -> {
        Object item = ev.getSelectedItem().getKey();
        ValueChangeEvent<Object> valueChangeEvent = new ValueChangeEvent<>(this, item);
        getEventDispatcher().dispatchEvent(valueChangeEvent);
      });

      registeredSelectValueChangeListener = true;
    }

    return registration;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <B> void onBind(BindingContext<B> context, Class<B> beanClass, String propertyName) {
    if (!context.getBinding(propertyName).getTransformer().isPresent()) {
      @SuppressWarnings("unchecked")
      Binding<T, Object, B, String> binding =
          (Binding<T, Object, B, String>) context.getBinding(getSelf());
      binding.setTransformer(new TypeTransformer<>());
    }
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
      throw new WebforjRuntimeException(e);
    }
  }
}
