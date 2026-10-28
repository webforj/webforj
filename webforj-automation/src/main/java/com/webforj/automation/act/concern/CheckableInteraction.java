package com.webforj.automation.act.concern;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CheckInteraction;
import com.webforj.automation.act.ClickInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.MouseOptions;
import com.webforj.component.Component;
import com.webforj.component.event.CheckEvent;
import com.webforj.component.event.ToggleEvent;
import com.webforj.component.event.UncheckEvent;
import com.webforj.component.optioninput.DwcOptionInput;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.component.optioninput.RadioButtonGroup;
import com.webforj.component.optioninput.event.RadioButtonGroupChangeEvent;
import com.webforj.concern.HasValue;
import java.util.HashMap;
import java.util.Map;

/**
 * Checks, unchecks and clicks every option input and every component whose value is a Boolean,
 * which a click toggles.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class CheckableInteraction extends AbstractComponentInteraction<Component>
    implements ClickInteraction<Component>, CheckInteraction<Component> {

  /**
   * Creates the interaction.
   */
  public CheckableInteraction() {
    super(Component.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSupported(Component component) {
    return component instanceof DwcOptionInput<?>
        || component instanceof HasValue<?, ?> hasValue && hasValue.getValue() instanceof Boolean;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void click(Component component, MouseOptions options) {
    setChecked(component, !isChecked(component));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isChecked(Component component) {
    return Boolean.TRUE.equals(((HasValue<?, ?>) component).getValue());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void check(Component component) {
    setChecked(component, true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void uncheck(Component component) {
    setChecked(component, false);
  }

  private static void setChecked(Component component, boolean checked) {
    if (!(component instanceof DwcOptionInput<?> optionInput)) {
      toObjectValue((HasValue<?, ?>) component).setValue(checked);
      return;
    }

    RadioButtonGroup group = component instanceof RadioButton radio ? radio.getButtonGroup() : null;
    final RadioButton previous = group == null ? null : group.getChecked();
    optionInput.setChecked(checked);

    if (checked) {
      fire(new CheckEvent(component, new HashMap<>()));
    } else {
      fire(new UncheckEvent(component, new HashMap<>()));
    }

    Map<String, Object> toggle = new HashMap<>();
    toggle.put("toggled", checked);
    fire(new ToggleEvent(component, toggle));

    if (checked && group != null && previous != component) {
      Map<String, Object> change = new HashMap<>();
      change.put("checked", component);
      change.put("unchecked", previous);
      fire(new RadioButtonGroupChangeEvent(group, change));
    }
  }

  @SuppressWarnings("unchecked")
  private static HasValue<?, Object> toObjectValue(HasValue<?, ?> hasValue) {
    return (HasValue<?, Object>) hasValue;
  }
}
