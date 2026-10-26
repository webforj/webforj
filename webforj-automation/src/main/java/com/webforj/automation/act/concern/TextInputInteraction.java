package com.webforj.automation.act.concern;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.TypeInteraction;
import com.webforj.component.Component;
import com.webforj.component.event.ModifyEvent;
import com.webforj.component.field.DwcField;
import com.webforj.component.list.ComboBox;
import com.webforj.concern.HasReadOnly;
import com.webforj.concern.HasText;
import com.webforj.concern.HasValue;
import java.util.HashMap;
import java.util.Map;

/**
 * Types into every component that reports its text as it changes, or that can be read only and
 * carries a value or a text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class TextInputInteraction extends AbstractComponentInteraction<Component>
    implements TypeInteraction<Component> {

  /**
   * Creates the interaction.
   */
  public TextInputInteraction() {
    super(Component.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSupported(Component component) {
    return hasModifyEvent(component) || component instanceof HasReadOnly<?>
        && (component instanceof HasValue<?, ?> || component instanceof HasText<?>);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void type(Component component, String text) {
    if (hasModifyEvent(component)) {
      ((HasText<?>) component).setText(text);

      Map<String, Object> data = new HashMap<>();
      data.put("text", text);
      fire(new ModifyEvent(component, data));

      return;
    }

    if (component instanceof HasValue<?, ?> hasValue) {
      toObjectValue(hasValue).setValue(text);
      return;
    }

    ((HasText<?>) component).setText(text);
  }

  private static boolean hasModifyEvent(Component component) {
    return component instanceof DwcField<?, ?> || component instanceof ComboBox;
  }

  @SuppressWarnings("unchecked")
  private static HasValue<?, Object> toObjectValue(HasValue<?, ?> hasValue) {
    return (HasValue<?, Object>) hasValue;
  }
}
