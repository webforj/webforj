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
@EventOptions(data = {@EventData(key = "detail", exp = "event.detail")})
public class LoginSubmitEvent extends ComponentEvent<Login> {

  /**
   * Creates an submit event.
   *
   * @param login the login component
   * @param eventMap the event map
   */
  public LoginSubmitEvent(Login login, Map<String, Object> eventMap) {
    super(login, eventMap);
    // Extract detail map and flatten into event map
    Object detail = eventMap.get("detail");
    if (detail instanceof Map) {
      eventMap.remove("detail");
      Map<String, Object> detailMap = (Map<String, Object>) detail;
      eventMap.putAll(detailMap);
    }
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
  public boolean isRememberMe() {
    return getData().get("remember-me").equals("on") ? true : false;
  }
}
