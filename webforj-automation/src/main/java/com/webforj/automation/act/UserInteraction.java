package com.webforj.automation.act;

import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.automation.find.ComponentTree;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.dialog.Dialog;
import com.webforj.component.drawer.Drawer;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.window.Window;
import com.webforj.component.window.event.WindowClickEvent;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasFocus;
import com.webforj.concern.HasReadOnly;
import com.webforj.concern.HasVisibility;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Performs what a user does with components, so listeners registered by the application run the way
 * they run in production.
 *
 * <p>
 * An interaction is a {@link Consumer} of the component, and {@link #perform(Component, Consumer)}
 * runs it after checking that a user can reach the component. Anyone writes one, as a lambda in
 * place, as a private method of a test or as a class of interactions for a component.
 * </p>
 *
 * <p>
 * The verbs of this class are shortcuts for the gestures several components share. Each component
 * answers them through the verb interfaces such as {@link ClickInteraction} and
 * {@link CloseInteraction}, and an application answers them for its own components the same way. A
 * verb no interaction performs on a component is rejected.
 * </p>
 *
 * <p>
 * A component has a class of interactions only when it has gestures of its own, and that class
 * holds only those gestures, for example {@code SliderInteraction.slide(int)}.
 * </p>
 *
 * <pre>{@code
 * UserInteraction.click(saveButton)
 * UserInteraction.perform(slider, SliderInteraction.slide(42))
 * UserInteraction.perform(rating, r -> UserInteraction.fireEvent(r, "rate", Map.of("stars", 4)))
 * }</pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class UserInteraction {

  private UserInteraction() {}

  /**
   * Clicks the given component.
   *
   * <p>
   * A component with a Boolean value toggles, a button or window fires its click event and every
   * component that exposes a click listener receives a click. A click also reaches the click
   * listeners of the components around the clicked one, up to and including the window, as it does
   * in the browser.
   * </p>
   *
   * @param component the component to click
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not react to
   *         clicks
   */
  public static void click(Component component) {
    click(component, new MouseOptions());
  }

  /**
   * Clicks the given component with the given pointer details.
   *
   * @param component the component to click
   * @param options where and how the pointer clicked
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not react to
   *         clicks
   */
  public static void click(Component component, MouseOptions options) {
    ensureUsable(component, false);
    Optional<ClickInteraction<Component>> interaction = find(ClickInteraction.class, component);
    boolean reachesElement = component instanceof ElementComposite;
    Window window = component.getWindow();
    boolean reachesWindow = window != null && window != component
        && !window.getEventListeners(WindowClickEvent.class).isEmpty();
    if (interaction.isEmpty() && !reachesElement && !reachesWindow) {
      throw new InteractionRejectedException(component, "does not react to clicks");
    }

    interaction.ifPresent(found -> found.click(component, options));
    if (reachesElement) {
      fireEvent(component, "click", createElementClickData(options));
    }

    // the click bubbles through the elements around the component, as it does in the browser
    for (Component holder : AbstractComponentInteraction.findHolders(component)) {
      for (Component bound = holder; bound instanceof Composite<?> && bound != window; bound =
          ComponentUtil.getBoundComponent(bound)) {
        if (bound instanceof ElementComposite) {
          fireEvent(bound, "click", createElementClickData(options));
        }
      }
    }

    if (reachesWindow) {
      ClickInteraction<Component> windowInteraction =
          require(ClickInteraction.class, window, "does not react to clicks");
      windowInteraction.click(window, createWindowOptions(options, component));
    }
  }

  /**
   * Clicks the given item of the given component, for example the node of a tree or the row of a
   * table.
   *
   * @param component the component holding the item
   * @param item the item to click
   *
   * @throws IllegalStateException if the component cannot be interacted with or has no items to
   *         click
   */
  public static void click(Component component, Object item) {
    click(component, item, new MouseOptions());
  }

  /**
   * Clicks the given item of the given component with the given pointer details.
   *
   * @param component the component holding the item
   * @param item the item to click
   * @param options where and how the pointer clicked
   *
   * @throws IllegalStateException if the component cannot be interacted with or has no items to
   *         click
   */
  public static void click(Component component, Object item, MouseOptions options) {
    ensureUsable(component, false);
    ItemClickInteraction<Component, Object> interaction =
        require(ItemClickInteraction.class, component, "has no items a user clicks");

    performOn(component, item, () -> interaction.click(component, item, options));
  }

  /**
   * Double clicks the given item of the given component.
   *
   * @param component the component holding the item
   * @param item the item to double click
   *
   * @throws IllegalStateException if the component cannot be interacted with or has no items to
   *         double click
   */
  public static void doubleClick(Component component, Object item) {
    doubleClick(component, item, new MouseOptions().setClickCount(2));
  }

  /**
   * Double clicks the given item of the given component with the given pointer details.
   *
   * @param component the component holding the item
   * @param item the item to double click
   * @param options where and how the pointer clicked
   *
   * @throws IllegalStateException if the component cannot be interacted with or has no items to
   *         double click
   */
  public static void doubleClick(Component component, Object item, MouseOptions options) {
    ensureUsable(component, false);
    DoubleClickInteraction<Component, Object> interaction =
        require(DoubleClickInteraction.class, component, "has no items a user double clicks");

    performOn(component, item, () -> interaction.doubleClick(component, item, options));
  }

  /**
   * Types the given text into the given component, replacing its current text.
   *
   * <p>
   * A component accepts typed text when it reports its text as it changes, or when it can be read
   * only and carries a value or a text.
   * </p>
   *
   * @param component the component to type into
   * @param text the text to type
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not accept
   *         typed text
   */
  public static void type(Component component, String text) {
    TypeInteraction<Component> interaction =
        require(TypeInteraction.class, component, "does not accept typed text");
    ensureUsable(component, true);

    interaction.type(component, text);
  }

  /**
   * Presses the given key while the given component has the focus.
   *
   * @param component the component to press the key on
   * @param key the key to press
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not react to
   *         keys
   */
  public static void keypress(Component component, KeypressEvent.Key key) {
    keypress(component, key, new KeyOptions());
  }

  /**
   * Presses the given key with the given modifier keys held.
   *
   * @param component the component to press the key on
   * @param key the key to press
   * @param options the modifier keys held
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not react to
   *         keys
   */
  public static void keypress(Component component, KeypressEvent.Key key, KeyOptions options) {
    KeypressInteraction<Component> interaction =
        require(KeypressInteraction.class, component, "does not react to keys");
    ensureUsable(component, true);

    interaction.keypress(component, key, options);
  }

  /**
   * Moves the focus onto the given component.
   *
   * @param component the component to focus
   *
   * @throws IllegalStateException if the component cannot be interacted with
   */
  public static void focus(HasFocus<?> component) {
    Component target = (Component) component;
    ensureUsable(target, false);
    FocusInteraction<Component> interaction =
        require(FocusInteraction.class, target, "cannot hold the focus");

    interaction.focus(target);
  }

  /**
   * Checks the given component. A checked component stays as it is.
   *
   * <p>
   * A component can be checked when its value is a Boolean.
   * </p>
   *
   * @param component the component to check
   *
   * @throws IllegalStateException if the component cannot be interacted with or cannot be checked
   */
  public static void check(Component component) {
    CheckInteraction<Component> interaction =
        require(CheckInteraction.class, component, "cannot be checked");
    ensureUsable(component, false);

    if (!interaction.isChecked(component)) {
      interaction.check(component);
    }
  }

  /**
   * Unchecks the given component. An unchecked component stays as it is.
   *
   * @param component the component to uncheck
   *
   * @throws IllegalStateException if the component cannot be interacted with or cannot be checked
   */
  public static void uncheck(Component component) {
    CheckInteraction<Component> interaction =
        require(CheckInteraction.class, component, "cannot be checked");
    ensureUsable(component, false);

    if (interaction.isChecked(component)) {
      interaction.uncheck(component);
    }
  }

  /**
   * Selects the given item in the given component.
   *
   * <p>
   * A webforJ component selects what it holds, a list its items, a table its rows, a tree its nodes
   * and a tabbed pane its tabs. Any other component selects through the selection contracts of
   * webforJ it implements.
   * </p>
   *
   * @param component the component to select in
   * @param item the item to select
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not select
   *         items
   */
  public static void select(Component component, Object item) {
    ensureUsable(component, false);
    SelectInteraction<Component, Object> interaction =
        require(SelectInteraction.class, component, "does not select items");

    performOn(component, item, () -> interaction.select(component, item));
  }

  /**
   * Selects the item at the given index in the given component.
   *
   * <p>
   * Lists select by index, and so does any component that implements the selection contracts of
   * webforJ.
   * </p>
   *
   * @param component the component to select in
   * @param index the index to select
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not select by
   *         index
   */
  public static void selectIndex(Component component, int index) {
    ensureUsable(component, false);
    SelectIndexInteraction<Component> interaction =
        require(SelectIndexInteraction.class, component, "does not select by index");

    interaction.selectIndex(component, index);
  }

  /**
   * Selects the item with the given key in the given component.
   *
   * <p>
   * Lists and tables select by key, and so does any component that implements the selection
   * contracts of webforJ.
   * </p>
   *
   * @param component the component to select in
   * @param key the key of the item to select
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not select by
   *         key
   */
  public static void selectKey(Component component, Object key) {
    ensureUsable(component, false);
    SelectKeyInteraction<Component> interaction =
        require(SelectKeyInteraction.class, component, "does not select by key");

    interaction.selectKey(component, key);
  }

  /**
   * Deselects the given item in the given component. An item that is not selected stays as it is.
   *
   * @param component the component to deselect in
   * @param item the item to deselect
   *
   * @throws IllegalStateException if the component cannot be interacted with or does not deselect
   *         items
   */
  public static void deselect(Component component, Object item) {
    ensureUsable(component, false);
    DeselectInteraction<Component, Object> interaction =
        require(DeselectInteraction.class, component, "does not deselect items");

    performOn(component, item, () -> interaction.deselect(component, item));
  }

  /**
   * Opens the given component, for example an accordion panel or the dropdown of a list.
   *
   * @param component the component to open
   *
   * @throws IllegalStateException if the component cannot be interacted with or cannot be opened by
   *         a user
   */
  public static void open(Component component) {
    ensureUsable(component, false);
    OpenInteraction<Component> interaction =
        require(OpenInteraction.class, component, "cannot be opened by a user");

    interaction.open(component);
  }

  /**
   * Closes the given component, for example an alert, a dialog or a drawer.
   *
   * @param component the component to close
   *
   * @throws IllegalStateException if the component cannot be interacted with or cannot be closed by
   *         a user
   */
  public static void close(Component component) {
    ensureUsable(component, false);
    CloseInteraction<Component> interaction =
        require(CloseInteraction.class, component, "cannot be closed by a user");

    interaction.close(component);
  }

  /**
   * Cancels the given component, for example a login or an upload.
   *
   * @param component the component to cancel
   *
   * @throws IllegalStateException if the component cannot be interacted with or cannot be cancelled
   *         by a user
   */
  public static void cancel(Component component) {
    ensureUsable(component, false);
    CancelInteraction<Component> interaction =
        require(CancelInteraction.class, component, "cannot be cancelled by a user");

    interaction.cancel(component);
  }

  /**
   * Performs the given interaction on the given component after checking that a user can reach it.
   *
   * <p>
   * The interaction is a {@link Consumer} of the component, a shipped one such as
   * {@code SliderInteraction.slide(int)} or one written in place. An interaction a user cannot
   * perform for a reason of its own, for example on a read only component, throws
   * {@link InteractionRejectedException}.
   * </p>
   *
   * <pre>{@code
   * UserInteraction.perform(slider, SliderInteraction.slide(42))
   * UserInteraction.perform(rating, r -> UserInteraction.fireEvent(r, "rate", Map.of("stars", 4)))
   * }</pre>
   *
   * @param <C> the component type
   * @param component the component to interact with
   * @param interaction what the user does with the component
   *
   * @throws IllegalStateException if the component cannot be interacted with
   */
  public static <C extends Component> void perform(C component, Consumer<C> interaction) {
    Objects.requireNonNull(component, "The component cannot be null");
    Objects.requireNonNull(interaction, "The interaction cannot be null");
    ensureUsable(component, false);

    interaction.accept(component);
  }

  /**
   * Fires the given event on its component, as if the browser sent it.
   *
   * <p>
   * An event a user does not cause, for example the progress of an upload or a dialog reporting
   * that it opened, is built with the constructor of the event and fired as it is. The data of the
   * event is what the event reads through its getters.
   * </p>
   *
   * @param event the event to fire
   *
   * @throws IllegalStateException if the component of the event is detached or delivers no events
   * @throws IllegalArgumentException if the component does not fire the event class
   */
  public static void fireEvent(ComponentEvent<?> event) {
    ensureAttached(event.getComponent());
    ComponentUtil.fireComponentEvent(event.getComponent(), event);
  }

  /**
   * Fires an event with the given name on the given component, as if the user caused it.
   *
   * <p>
   * The name is the one the component listens to, for example the name passed to an element's
   * {@code addEventListener}. Listeners registered for the name receive the given data.
   * </p>
   *
   * @param component the component to fire the event on
   * @param eventName the event name the component listens to
   * @param data the event data
   *
   * @throws IllegalStateException if the component is detached
   * @throws IllegalArgumentException if the component does not listen to events by name
   */
  public static void fireEvent(Component component, String eventName, Map<String, Object> data) {
    Objects.requireNonNull(eventName, "The event name cannot be null");
    ensureAttached(component);
    Element element = getEventElement(component);
    ComponentUtil.fireComponentEvent(element,
        new ElementEvent(element, data, eventName, 0, new ElementEventOptions()));
  }

  private static <I extends ComponentInteraction<?>> I require(Class<?> verb, Component component,
      String reason) {
    return UserInteraction.<I>find(verb, component)
        .orElseThrow(() -> new InteractionRejectedException(component, reason));
  }

  @SuppressWarnings("unchecked")
  private static <I extends ComponentInteraction<?>> Optional<I> find(Class<?> verb,
      Component component) {
    return ComponentInteractionRegistry.getCurrent()
        .find((Class<ComponentInteraction<?>>) verb, component).map(found -> (I) found);
  }

  private static Map<String, Object> createElementClickData(MouseOptions options) {
    Map<String, Object> data = new HashMap<>();
    data.put("screenX", options.getScreenX());
    data.put("screenY", options.getScreenY());
    data.put("clientX", options.getX());
    data.put("clientY", options.getY());
    data.put("detail", options.getClickCount());
    data.put("button", options.getButton());
    data.put("ctrlKey", options.isControlKey());
    data.put("shiftKey", options.isShiftKey());
    data.put("altKey", options.isAltKey());
    data.put("metaKey", options.isMetaKey());

    return data;
  }

  private static MouseOptions createWindowOptions(MouseOptions options, Component original) {
    return new MouseOptions().setButton(options.getButton()).setScreenX(options.getScreenX())
        .setScreenY(options.getScreenY()).setX(options.getX()).setY(options.getY())
        .setClickCount(options.getClickCount()).setAltKey(options.isAltKey())
        .setControlKey(options.isControlKey()).setShiftKey(options.isShiftKey())
        .setMetaKey(options.isMetaKey()).setOriginalComponent(original);
  }

  private static void performOn(Component component, Object item, Runnable action) {
    try {
      action.run();
    } catch (ClassCastException e) {
      throw new InteractionRejectedException(component,
          "does not hold items of type " + item.getClass().getSimpleName());
    }
  }

  private static Element getEventElement(Component component) {
    if (component instanceof Element element) {
      return element;
    }

    if (component instanceof Composite<?>
        && ComponentUtil.getBoundComponent(component) instanceof Element element) {
      return element;
    }

    throw new IllegalArgumentException(component.getClass().getSimpleName()
        + " does not listen to events by name. Name the event class it exposes to its listeners.");
  }

  private static void ensureAttached(Component component) {
    if (!component.isAttached()) {
      throw new IllegalStateException("The component is not attached. Add it to the context first."
          + System.lineSeparator() + ComponentTree.renderAll());
    }
  }

  private static void ensureUsable(Component component, boolean requireEditable) {
    ensureAttached(component);

    for (Component holder : AbstractComponentInteraction.findHolders(component).reversed()) {
      Optional<String> state = findUnreachableState(holder);
      if (state.isPresent()) {
        throw new InteractionRejectedException(component, "is inside the " + state.get() + " "
            + holder.getClass().getSimpleName() + " and a user cannot reach it");
      }
    }

    if (component instanceof HasVisibility<?> hasVisibility && !hasVisibility.isVisible()) {
      throw new InteractionRejectedException(component,
          "is hidden and a user cannot interact with it");
    }

    if (component instanceof HasEnablement<?> hasEnablement && !hasEnablement.isEnabled()) {
      throw new InteractionRejectedException(component,
          "is disabled and a user cannot interact with it");
    }

    if (requireEditable && component instanceof HasReadOnly<?> hasReadOnly
        && hasReadOnly.isReadOnly()) {
      throw new InteractionRejectedException(component,
          "is read only and a user cannot interact with it");
    }
  }

  private static Optional<String> findUnreachableState(Component holder) {
    if (holder instanceof HasVisibility<?> hasVisibility && !hasVisibility.isVisible()) {
      return Optional.of("hidden");
    }

    if (holder instanceof Dialog dialog && !dialog.isOpened()
        || holder instanceof Drawer drawer && !drawer.isOpened()) {
      return Optional.of("closed");
    }

    if (holder instanceof HasEnablement<?> hasEnablement && !hasEnablement.isEnabled()) {
      return Optional.of("disabled");
    }

    return Optional.empty();
  }
}
