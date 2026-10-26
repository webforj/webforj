package com.webforj.automation.act.component.window;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ClickInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.window.Window;
import com.webforj.component.window.event.WindowClickEvent;
import java.util.Map;

/**
 * Answers the {@code click} verb of {@link UserInteraction} for windows.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class WindowInteraction extends AbstractComponentInteraction<Window>
    implements ClickInteraction<Window> {

  /**
   * Creates the interaction.
   */
  public WindowInteraction() {
    super(Window.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void click(Window window, MouseOptions options) {
    Map<String, Object> data = mouseData(window, options);
    data.put("clickCount", options.getClickCount());
    fire(new WindowClickEvent(window, data));
  }
}
