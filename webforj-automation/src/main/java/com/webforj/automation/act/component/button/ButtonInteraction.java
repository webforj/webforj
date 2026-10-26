package com.webforj.automation.act.component.button;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ClickInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.button.DwcButton;
import com.webforj.component.button.event.ButtonClickEvent;
import java.util.HashMap;
import java.util.Map;

/**
 * Answers the {@code click} verb of {@link UserInteraction} for buttons.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class ButtonInteraction extends AbstractComponentInteraction<DwcButton<?>>
    implements ClickInteraction<DwcButton<?>> {

  /**
   * Creates the interaction.
   */
  public ButtonInteraction() {
    super(DwcButton.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void click(DwcButton<?> button, MouseOptions options) {
    Map<String, Object> data = new HashMap<>();
    data.put("x", (double) options.getX());
    data.put("y", (double) options.getY());
    fire(new ButtonClickEvent(button, data));
  }
}
