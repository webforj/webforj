package com.webforj.automation.act.component.alert;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CloseInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.component.alert.Alert;
import com.webforj.component.alert.event.AlertCloseEvent;
import java.util.HashMap;

/**
 * Closes alerts through their close button.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class AlertInteraction extends AbstractComponentInteraction<Alert>
    implements CloseInteraction<Alert> {

  /**
   * Creates the interaction.
   */
  public AlertInteraction() {
    super(Alert.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close(Alert alert) {
    if (!alert.isClosable()) {
      throw reject(alert, "has no close button and a user cannot close it");
    }

    alert.close();
    fire(new AlertCloseEvent(alert, new HashMap<>()));
  }
}
