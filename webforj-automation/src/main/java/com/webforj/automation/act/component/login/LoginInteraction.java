package com.webforj.automation.act.component.login;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CancelInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.component.login.Login;
import com.webforj.component.login.event.LoginCancelEvent;
import com.webforj.component.login.event.LoginSubmitEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Submits and cancels logins, closing a login that closes on its own and disabling it otherwise.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class LoginInteraction extends AbstractComponentInteraction<Login>
    implements CancelInteraction<Login> {
  private static final String USERNAME = "username";
  private static final String PASSWORD = "password";
  private static final String REMEMBER_ME = "rememberMe";

  /**
   * Creates the interaction.
   */
  public LoginInteraction() {
    super(Login.class);
  }

  /**
   * Returns the interaction that fills the form of a login with the given values and signs in.
   *
   * <p>
   * The values are the fields of the form by name, {@code username}, {@code password},
   * {@code rememberMe} as a Boolean and every field the application added. The username, the
   * password and remember me are filled into the form before it is sent, as a user types them.
   * </p>
   *
   * <pre>{@code
   * UserInteraction.perform(login, LoginInteraction.submit(Map.of("username", "ada")))
   * }</pre>
   *
   * @param values the values the user entered, by the name of their field
   *
   * @return the interaction to perform on the login
   */
  public static Consumer<Login> submit(Map<String, Object> values) {
    return login -> {
      Map<String, Object> detail = new HashMap<>(values);
      detail.remove(REMEMBER_ME);
      detail.putIfAbsent(USERNAME, "");
      detail.putIfAbsent(PASSWORD, "");
      boolean rememberMe = Boolean.TRUE.equals(values.get(REMEMBER_ME));
      detail.put("remember-me", rememberMe ? "on" : "off");
      login.setUsername(String.valueOf(detail.get(USERNAME)));
      login.setPassword(String.valueOf(detail.get(PASSWORD)));
      login.setRememberme(rememberMe);
      closeOrDisable(login);

      Map<String, Object> data = new HashMap<>();
      data.put("detail", detail);
      fire(new LoginSubmitEvent(login, data));
    };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancel(Login login) {
    String cancel = login.getI18n().getCancel();
    if (cancel == null || cancel.isBlank()) {
      throw reject(login, "has no cancel button and a user cannot cancel it");
    }

    closeOrDisable(login);
    fire(new LoginCancelEvent(login, new HashMap<>()));
  }

  private static void closeOrDisable(Login login) {
    if (login.isAutoClose()) {
      login.close();
    } else {
      login.setEnabled(false);
    }
  }
}
