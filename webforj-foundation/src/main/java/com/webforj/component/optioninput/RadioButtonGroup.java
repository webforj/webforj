package com.webforj.component.optioninput;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.Component;
import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.component.optioninput.event.RadioButtonGroupChangeEvent;
import com.webforj.component.optioninput.sink.RadioButtonGroupChangeSink;
import com.webforj.component.window.Window;
import com.webforj.concern.HasClientValidationStyle;
import com.webforj.data.concern.FocusAcceptorAware;
import com.webforj.data.concern.ValueAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.validation.InvalidAware;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * A class representing a RadioButtonGroup, which is a collection of radio buttons that are mutually
 * exclusive within the group. Only one radio button can be selected at a time.
 *
 * <p>
 * Radio buttons are UI components that allow users to select a single option from a set of choices.
 * The RadioButtonGroup class provides methods for adding and removing radio buttons and retrieving
 * the currently selected radio button
 * </p>
 *
 * <p>
 * Example usage:
 *
 * <pre>{@code
 * RadioButton option1 = new RadioButton("Option 1");
 * RadioButton option2 = new RadioButton("Option 2", true);
 * RadioButton option3 = new RadioButton("Option 3");
 *
 * RadioButtonGroup group = new RadioButtonGroup("Options", option1, option2, option3);
 *
 * // Add the radio buttons to a UI window
 * window.add(group);
 *
 * // Retrieve the currently selected radio button
 * RadioButton selected = group.getChecked();
 * }</pre>
 * </p>
 *
 * @see RadioButton
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public final class RadioButtonGroup extends Component implements Iterable<RadioButton>,
    InvalidAware<RadioButtonGroup>, ValueAware<RadioButtonGroup, String>,
    HasClientValidationStyle<RadioButtonGroup>, FocusAcceptorAware<RadioButtonGroup> {
  private final List<RadioButton> buttons = new CopyOnWriteArrayList<>();
  private BBjRadioGroup group;
  private Window window;
  private String name;
  private boolean registeredChangeValueChangeListener = false;

  private EventDispatcher dispatcher = new EventDispatcher();
  private EventSinkListenerRegistry<RadioButtonGroupChangeEvent> changedEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new RadioButtonGroupChangeSink(this, dispatcher),
          RadioButtonGroupChangeEvent.class);

  /**
   * Creates a RadioButtonGroup with a name and a list of RadioButtons.
   *
   * @param name the name of the RadioButtonGroup.
   * @param buttons the list of RadioButtons to add.
   */
  public RadioButtonGroup(String name, RadioButton... buttons) {
    setName(name);
    add(buttons);
  }

  /**
   * Creates a RadioButtonGroup with a name.
   *
   * @param name the name of the RadioButtonGroup.
   */
  public RadioButtonGroup(String name) {
    this(name, new RadioButton[0]);
  }

  /**
   * Creates a RadioButtonGroup with a list of RadioButtons.
   *
   * @param buttons the list of RadioButtons to add.
   */
  public RadioButtonGroup(RadioButton... buttons) {
    this(UUID.randomUUID().toString(), buttons);
  }

  /**
   * Creates a RadioButtonGroup.
   */
  public RadioButtonGroup() {
    this(UUID.randomUUID().toString(), new RadioButton[0]);
  }

  /**
   * Adds a RadioButton or a list of RadioButtons to the RadioButtonGroup.
   *
   * @param buttons the RadioButton or the list of RadioButtons to add.
   * @return the component itself.
   */
  public RadioButtonGroup add(RadioButton... buttons) {
    this.buttons.addAll(List.of(buttons));

    for (RadioButton button : buttons) {
      button.setButtonGroup(this);

      if (this.group != null) {
        // if the button is not already attached to a window then we add it to the window.
        if (Boolean.FALSE.equals(button.isAttached())) {
          this.window.add(button);
        }

        // add the button to the group
        try {
          BBjControl buttonControl = ComponentAccessor.getDefault().getControl(button);
          group.add((BBjRadioButton) buttonControl);
        } catch (IllegalAccessException | BBjException e) {
          throw new WebforjRuntimeException(
              "Failed to add the BBjRadioButton to the BBjRadioGroup.", e);
        }
      }
    }

    return this;
  }

  /**
   * Removes the RadioButton given as an Argument.
   *
   * @return the component itself.
   */
  public RadioButtonGroup remove(RadioButton... buttons) {
    for (RadioButton button : buttons) {
      button.setButtonGroup(null);
      this.buttons.remove(button);

      // remove the button from the group
      if (this.group != null) {
        try {
          BBjControl buttonControl = ComponentAccessor.getDefault().getControl(button);
          group.remove((BBjRadioButton) buttonControl);
        } catch (IllegalAccessException | BBjException e) {
          throw new WebforjRuntimeException(
              "Failed to remove the BBjRadioButton from the BBjRadioGroup.", e);
        }
      }
    }

    return this;
  }

  /**
   * Gets the checked RadioButton from RadioButtonGroup.
   *
   * @return The checked RadioButton.
   */
  public RadioButton getChecked() {
    // if the group is not created yet, then we loop over the buttons and check if one of them is
    // checked and return the last one that is checked.
    if (group == null) {
      Optional<RadioButton> lastChecked =
          buttons.stream().filter(x -> x.getChecked()).reduce((a, b) -> b);

      if (lastChecked.isPresent()) {
        return lastChecked.get();
      }

      return null;
    } else {
      // if the group is created then we get the checked button from the group and return the
      // corresponding RadioButton.
      try {
        BBjControl currentChecked = group.getSelected();
        if (currentChecked == null) {
          return null;
        }

        for (RadioButton button : buttons) {
          BBjControl buttonControl = ComponentAccessor.getDefault().getControl(button);
          if (buttonControl != null && (buttonControl.getID() == currentChecked.getID())) {
            return button;
          }
        }
      } catch (BBjException | IllegalAccessException e) {
        throw new WebforjRuntimeException("Failed to get the checked BBjRadioButton.", e);
      }
    }

    return null;
  }

  /**
   * Gets the name of the RadioButtonGroup.
   *
   * @return the name of the RadioButtonGroup.
   */
  @Override
  public String getName() {
    return this.name;
  }

  /**
   * Sets the name of the RadioButtonGroup.
   *
   * @param name the name of the RadioButtonGroup.
   * @return the component itself.
   */
  @Override
  public RadioButtonGroup setName(String name) {
    if (this.group != null) {
      try {
        this.group.setName(name);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.name = name;

    return this;
  }

  /**
   * Gets the list of tracked radio buttons.
   *
   * @return the list of added radio buttons.
   */
  public List<RadioButton> getRadioButtons() {
    return Collections.unmodifiableList(buttons);
  }

  /**
   * Alias for {@link #getRadioButtons()}.
   *
   * @return the list of added radio buttons.
   */
  public List<RadioButton> getButtons() {
    return getRadioButtons();
  }

  /**
   * Sets the value of the RadioButtonGroup.
   *
   * <p>
   * The method will check the radio button with the given name and uncheck the rest of the radio
   * buttons.
   * </p>
   *
   * @param name the name of the RadioButton to set as checked.
   * @return the component itself.
   */
  @Override
  public RadioButtonGroup setValue(String name) {
    buttons.stream().filter(x -> x.getName().equals(name)).findFirst()
        .ifPresent(x -> x.setChecked(true));

    return this;
  }

  /**
   * Gets the value of the RadioButtonGroup.
   *
   * <p>
   * The value of the RadioButtonGroup is the name of the checked RadioButton.
   * </p>
   *
   * @return the value of the RadioButtonGroup.
   */
  @Override
  public String getValue() {
    return Optional.ofNullable(getChecked()).map(RadioButton::getName).orElse(null);
  }

  /**
   * Sets the invalid state of the RadioButtonGroup.
   *
   * <p>
   * If the RadioButtonGroup is invalid, then the invalid state is set to true for the first
   * RadioButton in the group if none is checked or the checked RadioButton otherwise.
   * </p>
   *
   * @param invalid true if the RadioButtonGroup is invalid, false otherwise.
   * @return the component itself.
   */
  @Override
  public RadioButtonGroup setInvalid(boolean invalid) {
    getCheckedOrFirst().ifPresent(x -> x.setInvalid(invalid));
    return this;
  }

  /**
   * Checks if the RadioButtonGroup is invalid.
   *
   * <p>
   * The RadioButtonGroup is invalid if one of the RadioButtons is invalid.
   * </p>
   *
   * @return true if the RadioButtonGroup is invalid, false otherwise.
   */
  @Override
  public boolean isInvalid() {
    return getCheckedOrFirst().map(RadioButton::isInvalid).orElse(false);
  }

  /**
   * Sets the invalid message of the RadioButtonGroup.
   *
   * <p>
   * If the RadioButtonGroup is invalid, then the invalid message is set for the first RadioButton
   * in the group.
   * </p>
   *
   * @param message the invalid message of the RadioButtonGroup.
   * @return the component itself.
   */
  @Override
  public RadioButtonGroup setInvalidMessage(String message) {
    getCheckedOrFirst().ifPresent(x -> x.setInvalidMessage(message));
    return this;
  }

  /**
   * Gets the invalid message of the RadioButtonGroup.
   *
   * <p>
   * The invalid message of the RadioButtonGroup is the invalid message of the first RadioButton in
   * the group.
   * </p>
   *
   * @return the invalid message of the RadioButtonGroup.
   */
  @Override
  public String getInvalidMessage() {
    return getCheckedOrFirst().map(RadioButton::getInvalidMessage).orElse(null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButtonGroup setValidationStyle(ValidationStyle validationStyle) {
    getRadioButtons().forEach(x -> x.setValidationStyle(validationStyle));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ValidationStyle getValidationStyle() {
    return getFirstRadioButton().map(RadioButton::getValidationStyle).orElse(null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButtonGroup focus() {
    getCheckedOrFirst().ifPresent(RadioButton::focus);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<RadioButton> iterator() {
    return Collections.unmodifiableList(buttons).iterator();
  }

  /**
   * Adds a {@link RadioButtonGroupChangeEvent} listener to the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<RadioButtonGroupChangeEvent> addChangeListener(
      EventListener<RadioButtonGroupChangeEvent> listener) {
    return changedEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addChangeListener(EventListener) addCheckedListener}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<RadioButtonGroupChangeEvent> onChange(
      EventListener<RadioButtonGroupChangeEvent> listener) {
    return addChangeListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<ValueChangeEvent<String>> addValueChangeListener(
      EventListener<ValueChangeEvent<String>> listener) {

    ListenerRegistration<ValueChangeEvent<String>> registration =
        dispatcher.addListener(ValueChangeEvent.class, listener);

    if (!registeredChangeValueChangeListener) {
      addChangeListener(ev -> {
        RadioButtonGroup source = (RadioButtonGroup) ev.getSource();
        ValueChangeEvent<String> valueChangeEvent =
            new ValueChangeEvent<>(source, ev.getChecked().getName());
        dispatcher.dispatchEvent(valueChangeEvent);
      });

      registeredChangeValueChangeListener = true;
    }

    return registration;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window p) {
    try {
      this.window = p;
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      group = w.addRadioGroup();
    } catch (IllegalAccessException | BBjException e) {
      throw new WebforjRuntimeException("Failed to create the BBjRadioGroup.", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();
    this.changedEventSinkListenerRegistry.attach();

    setName(this.name);

    if (!this.buttons.isEmpty()) {
      add(this.buttons.toArray(new RadioButton[0]));
    }
  }

  @Override
  protected void onDestroy() {
    // BBjRadioGroup has no destroy method.
    // TODO : Ask Jim about whether this method should be implemented or not.
  }

  private Optional<RadioButton> getCheckedOrFirst() {
    return Optional.ofNullable(getChecked()).or(this::getFirstRadioButton);
  }

  private Optional<RadioButton> getFirstRadioButton() {
    return buttons.stream().findFirst();
  }
}
