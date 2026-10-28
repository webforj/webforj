package com.webforj.automation.act.component.terminal;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.TypeInteraction;
import com.webforj.component.terminal.Terminal;
import com.webforj.component.terminal.event.TerminalDataEvent;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

/**
 * Types into terminals, which report the text as their data.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class TerminalInteraction extends AbstractComponentInteraction<Terminal>
    implements TypeInteraction<Terminal> {

  /**
   * Creates the interaction.
   */
  public TerminalInteraction() {
    super(Terminal.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void type(Terminal terminal, String text) {
    Map<String, Object> data = new HashMap<>();
    data.put("value", Base64.getEncoder().encodeToString(text.getBytes(StandardCharsets.UTF_8)));
    fire(new TerminalDataEvent(terminal, data));
  }
}
