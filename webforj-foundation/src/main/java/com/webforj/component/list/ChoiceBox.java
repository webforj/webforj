package com.webforj.component.list;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.list.event.ListSelectEvent;
import com.webforj.component.window.Window;
import com.webforj.concern.HasTheme;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Represents a ChoiceBox component that provides users with a list of options for selection.
 *
 * <p>
 * A ChoiceBox is a UI component used to present users with a list of options or choices. It allows
 * users to select one option from the list, typically by clicking on the ChoiceBox, which then
 * displays a dropdown list of available choices. Users can choose an option from this list, and the
 * selected option is then displayed in the ChoiceBox.
 * </p>
 *
 * <p>
 * ChoiceBoxes are commonly used for various purposes, such as selecting items from a menu, choosing
 * from a list of categories, or picking options from predefined sets. They enhance the user
 * experience by providing a visually organized way to make selections, especially when there are
 * multiple options to choose from.
 * </p>
 *
 * @see DwcSelectDropdown
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
public final class ChoiceBox extends DwcSelectDropdown<ChoiceBox>
    implements HasTheme<ChoiceBox, ButtonTheme> {

  /**
   * Constructs a new ChoiceBox.
   */
  public ChoiceBox() {
    super();
    configureLisType();
  }

  /**
   * Constructs a new ChoiceBox with the given label.
   *
   * @param label the label
   */
  public ChoiceBox(String label) {
    super(label);
    configureLisType();
  }

  /**
   * Constructs a new ChoiceBox with the given label and select listener.
   *
   * @param label the label of the component
   * @param selectListener the listener to be called when the user selects an item
   */
  public ChoiceBox(String label, EventListener<ListSelectEvent<Object>> selectListener) {
    super(label, selectListener);
    configureLisType();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public ChoiceBox setTheme(ButtonTheme theme) {
    setComponentTheme(theme);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public ButtonTheme getTheme() {
    return super.<ButtonTheme>getComponentTheme();
  }

  /**
   * Selects the item with the given text.
   *
   * @return the component itself
   */
  @Override
  public ChoiceBox setText(String text) {
    Objects.requireNonNull(text, "The text cannot be null");

    String needle = text.trim();
    getItems().stream().filter(item -> item.getText().equals(needle)).findFirst()
        .ifPresent(this::select);

    return this;
  }

  /**
   * Selects an item with the given key.
   *
   * @return the component itself
   */
  @Override
  public String getText() {
    return Optional.ofNullable(getSelectedItem()).map(ListItem::getText).orElse(null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autoValidate", "autoValidateOnLoad", "autoWasValidated",
        "buttonHeight", "disabled", "distance", "expanse", "hasFocus", "invalid", "invalidMessage",
        "itemLabel", "itemValue", "items", "label", "maxRowCount", "openHeight", "openWidth",
        "opened", "placement", "readonly", "renderItemPrefix", "selected", "skidding", "theme",
        "type", "typeToSelect", "typeToSelectCaseSensitive", "typeToSelectTimeout", "valid",
        "validationIcon", "validationPopoverDistance", "validationPopoverPlacement",
        "validationPopoverSkidding", "validationStyle", "validator"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window panel) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(panel);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addListButton(resolveControlId(w), "", flags));
    } catch (Exception e) {
      throw new WebforjRuntimeException("Failed to create the BBjListButton Control", e);
    }
  }

  private void configureLisType() {
    setDropdownType("ChoiceBox");
    setUnrestrictedProperty("buttonHeight", "");
  }
}
