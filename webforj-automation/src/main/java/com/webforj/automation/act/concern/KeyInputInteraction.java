package com.webforj.automation.act.concern;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.KeyOptions;
import com.webforj.automation.act.KeypressInteraction;
import com.webforj.component.Component;
import com.webforj.component.event.KeypressEvent;
import com.webforj.component.field.DwcField;
import com.webforj.component.list.ComboBox;
import java.util.HashMap;
import java.util.Map;

/**
 * Presses a key on every component that reports the keys pressed on it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class KeyInputInteraction extends AbstractComponentInteraction<Component>
    implements KeypressInteraction<Component> {

  /**
   * Creates the interaction.
   */
  public KeyInputInteraction() {
    super(Component.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isSupported(Component component) {
    return component instanceof DwcField<?, ?> || component instanceof ComboBox;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void keypress(Component component, KeypressEvent.Key key, KeyOptions options) {
    Map<String, Object> data = new HashMap<>();
    data.put("keyCode", key.getCode());
    data.put("altKey", options.isAltKey());
    data.put("cmdKey", options.isMetaKey());
    data.put("controlKey", options.isControlKey());
    data.put("shiftKey", options.isShiftKey());
    fire(new KeypressEvent(component, data));
  }
}
