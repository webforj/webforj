package org.dwcj.component.list;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.button.ButtonTheme;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.list.event.ListSelectEvent;
import org.dwcj.component.window.Window;
import org.dwcj.concern.HasTheme;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

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
    setDropdownType("ChoiceBox");
    setUnrestrictedProperty("buttonHeight", "");
  }

  /**
   * Constructs a new ChoiceBox with the given label.
   *
   * @param label the label
   */
  public ChoiceBox(String label) {
    super(label);
  }

  /**
   * Constructs a new ChoiceBox with the given label and select listener.
   *
   * @param label the label of the component
   * @param selectListener the listener to be called when the user selects an item
   */
  public ChoiceBox(String label, ComponentEventListener<ListSelectEvent> selectListener) {
    super(label, selectListener);
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
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window panel) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(panel);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addListButton("", flags));
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create the BBjListButton Control", e);
    }
  }
}
