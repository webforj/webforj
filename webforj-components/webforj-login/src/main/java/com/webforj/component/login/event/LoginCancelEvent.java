package com.webforj.component.login.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.login.Login;
import java.util.Map;

/**
 * Emitted when the cancel button is clicked.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName("dwc-login-cancel")
public class LoginCancelEvent extends ComponentEvent<Login> {

  /**
   * Creates an cancel event.
   *
   * @param login the login component
   * @param eventMap the event map
   */
  public LoginCancelEvent(Login login, Map<String, Object> eventMap) {
    super(login, eventMap);
  }
}
