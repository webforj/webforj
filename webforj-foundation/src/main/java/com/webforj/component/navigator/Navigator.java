package com.webforj.component.navigator;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.google.gson.annotations.SerializedName;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.Expanse;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.navigator.event.NavigatorChangeEvent;
import com.webforj.component.navigator.event.NavigatorMoveEvent;
import com.webforj.component.navigator.event.NavigatorMoveFirstEvent;
import com.webforj.component.navigator.event.NavigatorMoveLastEvent;
import com.webforj.component.navigator.event.NavigatorMoveNextEvent;
import com.webforj.component.navigator.event.NavigatorMovePreviousEvent;
import com.webforj.component.navigator.sink.NavigatorMoveFirstEventSink;
import com.webforj.component.navigator.sink.NavigatorMoveLastEventSink;
import com.webforj.component.navigator.sink.NavigatorMoveNextEventSink;
import com.webforj.component.navigator.sink.NavigatorMovePreviousEventSink;
import com.webforj.component.window.Window;
import com.webforj.concern.HasExpanse;
import com.webforj.data.Paginator;
import com.webforj.data.repository.Repository;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Objects;

/**
 * Ac customizable pagination component for navigating through data sets, supporting various
 * layouts.
 *
 * <p>
 * The Navigator component that allows for the pagination of items within a data set. It can be
 * configured to display various navigation controls such as first, last, next, and previous
 * buttons, along with page numbers or a quick jump field depending on the layout setting. It
 * supports automatic disabling of navigation buttons based on the current page and total items, and
 * offers customization options for text and tooltips for different parts of the navigator.
 * Additionally, it can be bound to a Paginator instance to manage the data set's pagination logic
 * and reflect changes in the navigation controls.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class Navigator extends DwcFocusableComponent<Navigator>
    implements HasExpanse<Navigator, Expanse> {

  /**
   * Describes the layout of the middle area of the Navigator.
   */
  public enum Layout {
    /** No layout will be rendered. */
    @SerializedName("none")
    NONE,

    /** The numbered buttons will be rendered. */
    @SerializedName("numbered")
    PAGES,

    /** Only the current page and the number of pages will be rendered. */
    @SerializedName("preview")
    PREVIEW,

    /** The quick jump input will be rendered. */
    @SerializedName("quick-jump")
    QUICK_JUMP
  }

  /**
   * Describes the part of the Navigator.
   */
  public enum Part {
    FIRST_BUTTON("First"), LAST_BUTTON("Last"), NEXT_BUTTON("Next"), PREVIOUS_BUTTON(
        "Previous"), PAGE_BUTTON("Page");

    private final String value;

    Part(String value) {
      this.value = value;
    }

    /**
     * Returns the value of the part.
     *
     * @return the value of the part
     **/
    public String getValue() {
      return value;
    }
  }

  private static final String PART_NULL_ERROR_MESSAGE = "Part cannot be null";

  private final ComponentEventSinkRegistry<NavigatorMoveFirstEvent> firstEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new NavigatorMoveFirstEventSink(this, getEventDispatcher()),
          NavigatorMoveFirstEvent.class);
  private final ComponentEventSinkRegistry<NavigatorMoveLastEvent> lastEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new NavigatorMoveLastEventSink(this, getEventDispatcher()),
          NavigatorMoveLastEvent.class);
  private final ComponentEventSinkRegistry<NavigatorMoveNextEvent> nextEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new NavigatorMoveNextEventSink(this, getEventDispatcher()),
          NavigatorMoveNextEvent.class);
  private final ComponentEventSinkRegistry<NavigatorMovePreviousEvent> previousEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(
          new NavigatorMovePreviousEventSink(this, getEventDispatcher()),
          NavigatorMovePreviousEvent.class);

  private Paginator paginator;
  private boolean autoDisable = false;
  private Layout layout = Layout.PREVIEW;
  private PropertyChangeListener lastPropertyChangeListener;
  private String label = "navigation";

  /**
   * Constructs a Navigator instance with the specified total number of items, page size, and
   * Layout.
   *
   * @param totalItems The total number of items to be paginated.
   * @param pageSize The number of items to display on each page.
   * @param layout The layout of the middle area of the Navigator.
   */
  public Navigator(int totalItems, int pageSize, Layout layout) {
    this(new Paginator(totalItems, pageSize), layout);
  }

  /**
   * Constructs a Navigator instance with the specified total number of items and page size.
   *
   * @param totalItems The total number of items to be paginated.
   * @param pageSize The number of items to display on each page.
   */
  public Navigator(int totalItems, int pageSize) {
    this(new Paginator(totalItems, pageSize));
  }

  /**
   * Constructs a Navigator instance with the specified total number of items and Layout.
   *
   * @param totalItems The total number of items to be paginated.
   * @param layout The layout of the middle area of the Navigator.
   */
  public Navigator(int totalItems, Layout layout) {
    this(new Paginator(totalItems), layout);
  }

  /**
   * Constructs a Navigator instance with the specified total number of items.
   *
   * @param totalItems The total number of items to be paginated.
   */
  public Navigator(int totalItems) {
    this(new Paginator(totalItems));
  }

  /**
   * Constructs a Navigator instance with the specified repository, page size and Layout.
   *
   * @param repository The repository containing the items to be paginated.
   * @param pageSize The number of items to display on each page.
   * @param layout The layout of the middle area of the Navigator.
   */
  public Navigator(Repository<?> repository, int pageSize, Layout layout) {
    this(new Paginator(repository, pageSize), layout);
  }

  /**
   * Constructs a Navigator instance with the specified repository and page size.
   *
   * @param repository The repository containing the items to be paginated.
   * @param pageSize The number of items to display on each page.
   */
  public Navigator(Repository<?> repository, int pageSize) {
    this(new Paginator(repository, pageSize));
  }

  /**
   * Constructs a Navigator instance with the specified repository and Layout.
   *
   * @param repository The repository containing the items to be paginated.
   * @param layout The layout of the middle area of the Navigator.
   */
  public Navigator(Repository<?> repository, Layout layout) {
    this(new Paginator(repository), layout);
  }

  /**
   * Constructs a Navigator instance with the specified repository.
   *
   * @param repository The repository containing the items to be paginated.
   */
  public Navigator(Repository<?> repository) {
    this(new Paginator(repository));
  }

  /**
   * Constructs a Navigator instance with the given paginator and Layout.
   *
   * @param paginator The paginator to be used.
   * @param layout The layout of the middle area of the Navigator.
   */
  public Navigator(Paginator paginator, Layout layout) {
    super();
    setPaginator(paginator);
    setLayout(layout);
    setExpanse(Expanse.MEDIUM);

    addMoveFirstEvent(this::handleChangeEvent);
    addMoveLastEvent(this::handleChangeEvent);
    addMoveNextEvent(this::handleChangeEvent);
    addMovePreviousEvent(this::handleChangeEvent);
  }

  /**
   * Constructs a Navigator instance with the given paginator.
   *
   * @param paginator The paginator to be used.
   */
  public Navigator(Paginator paginator) {
    this(paginator, Layout.PREVIEW);
  }

  /**
   * Constructs a Navigator instance with the specified text.
   *
   * @param text the text of the Navigator.
   */
  public Navigator(String text) {
    this();
    setText(text);
  }

  /**
   * Constructs a Navigator instance.
   */
  public Navigator() {
    this(new Paginator());
  }

  /**
   * When true, the first, previous, next, and last buttons will be auto disabled based on the total
   * items and the current page.
   *
   * @param autoDisable true to enable auto disable
   * @return the component itself.
   */
  public Navigator setAutoDisable(boolean autoDisable) {
    this.autoDisable = autoDisable;
    setUnrestrictedProperty("autoDisable", autoDisable);
    return this;
  }

  /**
   * Gets whether or not the first, previous, next, and last buttons will be auto disabled based on
   * the total items and the current page.
   *
   * @return true if auto disable is enabled
   */
  public boolean isAutoDisable() {
    return autoDisable;
  }

  /**
   * Sets the layout of the middle area of the Navigator.
   *
   * @param layout the layout of the middle area of the Navigator.
   * @return the component itself.
   */
  public Navigator setLayout(Layout layout) {
    this.layout = layout;
    setUnrestrictedProperty("layout", layout);
    return this;
  }

  /**
   * Gets the layout of the middle area of the Navigator.
   *
   * @return the layout of the middle area of the Navigator.
   */
  public Layout getLayout() {
    return layout;
  }

  /**
   * Sets the paginator of the Navigator.
   *
   * @param paginator the paginator of the Navigator.
   * @return the component itself.
   */
  public Navigator setPaginator(Paginator paginator) {
    Objects.requireNonNull(paginator, "Paginator cannot be null");

    if (lastPropertyChangeListener != null) {
      this.paginator.removePropertyChangeListener(lastPropertyChangeListener);
    }

    lastPropertyChangeListener = new PaginatorChangeListener();
    paginator.addPropertyChangeListener(lastPropertyChangeListener);
    this.paginator = paginator;

    setUnrestrictedProperty("totalItems", paginator.getTotalItems());
    setUnrestrictedProperty("current", paginator.getCurrent());
    setUnrestrictedProperty("size", paginator.getSize());
    setUnrestrictedProperty("max", paginator.getMax());

    return this;
  }

  /**
   * Sets the text of the Navigator component.
   *
   * <p>
   * Text expressions are evaluated as JavaScript expressions. The text expression is evaluated with
   * the following parameters:
   * <ul>
   * <li><code>page</code> - the current page number</li>
   * <li><code>current</code> - the currently selected page number</li>
   * <li><code>x</code> - an alias for the current page</li>
   * <li><code>startIndex</code> - The start index of the current page.</li>
   * <li><code>endIndex</code> - The end index of the current page.</li>
   * <li><code>totalItems</code> - The total number of items.</li>
   * <li><code>startPage</code> - The start page number.</li>
   * <li><code>endPage</code> - The end page number.</li>
   * <li><code>component</code> - The Navigator client component.</li>
   * </ul>
   *
   * For example, the following code will set page buttons text to <code>'0' + page</code>:
   *
   * <pre>
   * <code>
   *
   * navigator.setText("'0' + page", Part.PAGE_BUTTON);
   * </code>
   * </pre>
   *
   * </p>
   *
   * @param text the text to set
   * @param part the part of the Navigator.
   *
   * @return the component itself.
   */
  public Navigator setText(String text, Part part) {
    Objects.requireNonNull(text, "Text cannot be null");
    Objects.requireNonNull(part, PART_NULL_ERROR_MESSAGE);

    String prop = "text" + part.getValue();
    setUnrestrictedProperty(prop, text);
    return this;
  }

  /**
   * Gets the text of the Navigator part.
   *
   * @param part the part of the Navigator.
   *
   * @return the text of the Navigator part.
   */
  public String getText(Part part) {
    Objects.requireNonNull(part, PART_NULL_ERROR_MESSAGE);
    String prop = "text" + part.getValue();

    return (String) getProperty(prop);
  }

  /**
   * Sets the tooltip text of the Navigator part.
   *
   * @param tooltip the tooltip text to set
   * @param part the part of the Navigator.
   *
   * @return the component itself.
   */
  public Navigator setTooltipText(String tooltip, Part part) {
    Objects.requireNonNull(tooltip, "Tooltip cannot be null");
    Objects.requireNonNull(part, PART_NULL_ERROR_MESSAGE);

    String prop = "label" + part.getValue();
    setUnrestrictedProperty(prop, tooltip);
    return this;
  }

  /**
   * Gets the tooltip text of the Navigator part.
   *
   * @param part the part of the Navigator.
   *
   * @return the tooltip text of the Navigator part.
   */
  public String getTooltipText(Part part) {
    Objects.requireNonNull(part, PART_NULL_ERROR_MESSAGE);
    String prop = "label" + part.getValue();

    return (String) getProperty(prop);
  }

  /**
   * Sets the visibility of the Navigator part.
   *
   * @param visible true to make the part visible
   * @param part the part of the Navigator.
   *
   * @return the component itself.
   */
  public Navigator setVisible(boolean visible, Part part) {
    Objects.requireNonNull(part, PART_NULL_ERROR_MESSAGE);

    String prop = null;
    switch (part) {
      case FIRST_BUTTON:
        prop = "suppressFirst";
        break;
      case LAST_BUTTON:
        prop = "suppressLast";
        break;
      case NEXT_BUTTON:
        prop = "suppressNext";
        break;
      case PREVIOUS_BUTTON:
        prop = "suppressPrevious";
        break;
      case PAGE_BUTTON:
        setLayout(Layout.NONE);
        break;
      default:
        break;
    }

    if (prop != null) {
      setUnrestrictedProperty(prop, !visible);
    }

    return this;
  }

  /**
   * Gets the visibility of the Navigator part.
   *
   * @param part the part of the Navigator.
   *
   * @return true if the part is visible
   */
  public boolean isVisible(Part part) {
    Objects.requireNonNull(part, PART_NULL_ERROR_MESSAGE);

    String prop = null;
    switch (part) {
      case FIRST_BUTTON:
        prop = "suppressFirst";
        break;
      case LAST_BUTTON:
        prop = "suppressLast";
        break;
      case NEXT_BUTTON:
        prop = "suppressNext";
        break;
      case PREVIOUS_BUTTON:
        prop = "suppressPrevious";
        break;
      default:
        break;
    }

    if (prop != null) {
      return !(boolean) getProperty(prop);
    }

    if (part == Part.PAGE_BUTTON) {
      return getLayout() != Layout.NONE;
    }

    return false;
  }

  /**
   * The method will hide all main buttons, which include first, last, next, and previous buttons,
   * keeping the middle area as it is.
   *
   * @param hideMainButtons true to hide the main buttons
   * @return the component itself.
   */
  public Navigator setHideMainButtons(boolean hideMainButtons) {
    setVisible(!hideMainButtons, Part.FIRST_BUTTON);
    setVisible(!hideMainButtons, Part.LAST_BUTTON);
    setVisible(!hideMainButtons, Part.NEXT_BUTTON);
    setVisible(!hideMainButtons, Part.PREVIOUS_BUTTON);

    return this;
  }

  /**
   * Returns true if the main buttons are hidden.
   *
   * @return true if the main buttons are hidden
   */
  public boolean isHideMainButtons() {
    return !isVisible(Part.FIRST_BUTTON) && !isVisible(Part.LAST_BUTTON)
        && !isVisible(Part.NEXT_BUTTON) && !isVisible(Part.PREVIOUS_BUTTON);
  }

  /**
   * Sets label of the Navigator.
   *
   * <p>
   * The label is not rendered in the client side. It is used for accessibility purposes.
   * </p>
   *
   * @param label the label of the Navigator.
   * @return the component itself.
   */
  public Navigator setLabel(String label) {
    this.label = label;
    setUnrestrictedProperty("label", label);
    return this;
  }

  /**
   * Gets the label of the Navigator.
   *
   * @return the label of the Navigator.
   */
  public String getLabel() {
    return label;
  }

  /**
   * Gets the paginator of the Navigator.
   *
   * @return the paginator of the Navigator.
   */
  public Paginator getPaginator() {
    return paginator;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Navigator setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Expanse getExpanse() {
    return super.<Expanse>getComponentExpanse();
  }

  /**
   * Adds a {@link NavigatorMoveFirstEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NavigatorMoveFirstEvent> addMoveFirstEvent(
      EventListener<NavigatorMoveFirstEvent> listener) {
    return this.firstEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addMoveFirstEvent(EventListener) addButtonClickListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public ListenerRegistration<NavigatorMoveFirstEvent> onMoveFirst(
      EventListener<NavigatorMoveFirstEvent> listener) {
    return addMoveFirstEvent(listener);
  }

  /**
   * Adds a {@link NavigatorMoveLastEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NavigatorMoveLastEvent> addMoveLastEvent(
      EventListener<NavigatorMoveLastEvent> listener) {
    return this.lastEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addMoveLastEvent(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public ListenerRegistration<NavigatorMoveLastEvent> onMoveLast(
      EventListener<NavigatorMoveLastEvent> listener) {
    return addMoveLastEvent(listener);
  }

  /**
   * Adds a {@link NavigatorMoveNextEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NavigatorMoveNextEvent> addMoveNextEvent(
      EventListener<NavigatorMoveNextEvent> listener) {
    return this.nextEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addMoveNextEvent(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public ListenerRegistration<NavigatorMoveNextEvent> onMoveNext(
      EventListener<NavigatorMoveNextEvent> listener) {
    return addMoveNextEvent(listener);
  }

  /**
   * Adds a {@link NavigatorMovePreviousEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NavigatorMovePreviousEvent> addMovePreviousEvent(
      EventListener<NavigatorMovePreviousEvent> listener) {
    return this.previousEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addMovePreviousEvent(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public ListenerRegistration<NavigatorMovePreviousEvent> onMovePrevious(
      EventListener<NavigatorMovePreviousEvent> listener) {
    return addMovePreviousEvent(listener);
  }

  /**
   * Adds a {@link NavigatorChangeEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<NavigatorChangeEvent> addChangeEvent(
      EventListener<NavigatorChangeEvent> listener) {
    return getEventDispatcher().addListener(NavigatorChangeEvent.class, listener);
  }

  /**
   * Alias for {@link #addChangeEvent(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public ListenerRegistration<NavigatorChangeEvent> onChange(
      EventListener<NavigatorChangeEvent> listener) {
    return addChangeEvent(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    firstEventSinkListenerRegistry.attach();
    lastEventSinkListenerRegistry.attach();
    nextEventSinkListenerRegistry.attach();
    previousEventSinkListenerRegistry.attach();
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
      setControl(w.addNavigator(getText(), flags));
    } catch (Exception e) {
      throw new WebforjRuntimeException("Failed to create the BBjNavigator Control", e);
    }
  }

  /**
   * Handles navigation change events.
   *
   * @param event the navigation event
   */
  void handleChangeEvent(NavigatorMoveEvent event) {
    int current = event.getCurrent();
    NavigatorChangeEvent.Direction direction = null;

    if (event instanceof NavigatorMoveFirstEvent) {
      direction = NavigatorChangeEvent.Direction.FIRST;
    } else if (event instanceof NavigatorMoveLastEvent) {
      direction = NavigatorChangeEvent.Direction.LAST;
    } else if (event instanceof NavigatorMoveNextEvent) {
      direction = NavigatorChangeEvent.Direction.NEXT;
    } else if (event instanceof NavigatorMovePreviousEvent) {
      direction = NavigatorChangeEvent.Direction.PREVIOUS;
    }

    paginator.setCurrent(current);

    NavigatorChangeEvent navigatorChangeEvent = new NavigatorChangeEvent(this, direction, current,
        event.getStartIndex(), event.getEndIndex());

    getEventDispatcher().dispatchEvent(navigatorChangeEvent);
  }

  /**
   * A paginator change listener. When the paginator changes, the Navigator will be updated in the
   * client side.
   */
  private final class PaginatorChangeListener implements PropertyChangeListener {
    /**
     * {@inheritDoc}
     */
    @Override
    public void propertyChange(PropertyChangeEvent ev) {
      String property = ev.getPropertyName();
      setUnrestrictedProperty(property, ev.getNewValue());
    }
  }
}
