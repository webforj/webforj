package com.webforj.component.login.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.login.Login;
import java.util.Map;

/**
 * Emitted when the login form is submitted.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName("dwc-login-submit")
@EventOptions(data = {@EventData(key = "username", exp = "event.detail.username"),
    @EventData(key = "password", exp = "event.detail.password"),
    @EventData(key = "rememberme", exp = "event.detail.rememberme")})
public class LoginSubmitEvent extends ComponentEvent<Login> {

  /**
   * Creates an submit event.
   *
   * @param login the login component
   * @param eventMap the event map
   */
  public LoginSubmitEvent(Login login, Map<String, Object> eventMap) {
    super(login, eventMap);
  }

  /**
   * Gets the username submitted.
   *
   * @return the username
   */
  public String getUsername() {
    return (String) getData().get("username");
  }

  /**
   * Gets the password submitted.
   *
   * @return the password
   */
  public String getPassword() {
    return (String) getData().get("password");
  }

  /**
   * Gets the remember me submitted.
   *
   * @return the remember me
   */
  public boolean getRememberMe() {
    return (boolean) getData().get("rememberme");
  }
}
