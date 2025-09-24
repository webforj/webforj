package com.webforj.spring.security;

import com.google.gson.Gson;
import com.webforj.Page;
import java.util.HashMap;
import java.util.Map;

/**
 * Submits forms to Spring Security endpoints.
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public final class SpringSecurityFormSubmitter {

  /**
   * Form types.
   */
  public enum FormType {
    LOGIN, LOGOUT
  }

  private SpringSecurityFormSubmitter() {
    // Utility class
  }

  /**
   * Creates a login form builder.
   *
   * @param loginUrl the login URL
   * @return a LoginFormBuilder
   */
  public static LoginFormBuilder login(String loginUrl) {
    return new LoginFormBuilder(loginUrl);
  }

  /**
   * Creates a logout form builder.
   *
   * @param logoutUrl the logout URL
   * @return a LogoutFormBuilder
   */
  public static LogoutFormBuilder logout(String logoutUrl) {
    return new LogoutFormBuilder(logoutUrl);
  }

  /**
   * Creates a form builder for the specified form type and action URL.
   *
   * @param <T> the builder type
   * @param type the form type
   * @param action the form action URL
   * @return a typed builder instance
   */
  @SuppressWarnings("unchecked")
  public static <T> T builder(FormType type, String action) {
    return switch (type) {
      case LOGIN -> (T) new LoginFormBuilder(action);
      case LOGOUT -> (T) new LogoutFormBuilder(action);
    };
  }

  /**
   * Base form builder.
   */
  static class FormBuilder {
    protected final String action;
    protected final Map<String, String> fields = new HashMap<>();
    protected String method = "POST";

    FormBuilder(String action) {
      this.action = action;
    }

    /**
     * Gets the form action URL.
     *
     * @return the action URL
     */
    public String getAction() {
      return action;
    }

    /**
     * Gets the HTTP method.
     *
     * @return the HTTP method
     */
    public String getMethod() {
      return method;
    }

    /**
     * Gets all form fields.
     *
     * @return unmodifiable view of the fields map
     */
    public Map<String, String> getFields() {
      return Map.copyOf(fields);
    }

    /**
     * Gets the value of a specific field.
     *
     * @param name the field name
     * @return the field value, or null if not set
     */
    public String getField(String name) {
      return fields.get(name);
    }

    /**
     * Checks if a field is present.
     *
     * @param name the field name
     * @return true if the field is present
     */
    public boolean hasField(String name) {
      return fields.containsKey(name);
    }

    /**
     * Sets the HTTP method.
     *
     * @param method the HTTP method
     * @return this builder
     */
    public FormBuilder method(String method) {
      this.method = method;
      return this;
    }

    /**
     * Adds a field to the form.
     *
     * @param name field name
     * @param value field value
     * @return this builder
     */
    public FormBuilder addField(String name, String value) {
      fields.put(name, value);
      return this;
    }

    /**
     * Adds multiple fields to the form.
     *
     * @param fields field map
     * @return this builder
     */
    public FormBuilder addFields(Map<String, String> fields) {
      this.fields.putAll(fields);
      return this;
    }

    /**
     * Removes a field from the form.
     *
     * @param name field name
     * @return this builder
     */
    public FormBuilder removeField(String name) {
      fields.remove(name);
      return this;
    }

    /**
     * Clears all fields from the form.
     *
     * @return this builder
     */
    public FormBuilder clearFields() {
      fields.clear();
      return this;
    }

    /**
     * Submits the form.
     */
    public void submit() {
      // Create a data object with form configuration
      Map<String, Object> data = new HashMap<>();
      data.put("action", action);
      data.put("method", method);
      data.put("fields", fields);

      // Convert to JSON using Gson (proper escaping)
      Gson gson = new Gson();
      String jsonData = gson.toJson(data);

      // Create and submit the form
      String js = String.format("""
          (function() {
            var data = %s;
            var form = document.createElement('form');
            form.method = data.method;
            form.action = data.action;
            form.style.display = 'none';

            for (var key in data.fields) {
              if (data.fields.hasOwnProperty(key)) {
                var input = document.createElement('input');
                input.type = 'hidden';
                input.name = key;
                input.value = data.fields[key];
                form.appendChild(input);
              }
            }

            document.body.appendChild(form);
            form.submit();
          })();
          """, jsonData);

      Page.getCurrent().executeJsVoidAsync(js);
    }
  }

  /**
   * Login form builder.
   */
  public static class LoginFormBuilder extends FormBuilder {

    LoginFormBuilder(String action) {
      super(action);
    }

    /**
     * Sets the username field.
     *
     * @param username the username
     * @return this builder
     */
    public LoginFormBuilder username(String username) {
      addField("username", username);
      return this;
    }

    /**
     * Sets the password field.
     *
     * @param password the password
     * @return this builder
     */
    public LoginFormBuilder password(String password) {
      addField("password", password);
      return this;
    }

    /**
     * Enables or disables remember-me.
     *
     * @param rememberMe enable remember-me
     * @return this builder
     */
    public LoginFormBuilder rememberMe(boolean rememberMe) {
      if (rememberMe) {
        addField("remember-me", "on");
      } else {
        removeField("remember-me");
      }
      return this;
    }

    /**
     * Gets the username field value.
     *
     * @return the username, or null if not set
     */
    public String getUsername() {
      return getField("username");
    }

    /**
     * Gets the password field value.
     *
     * @return the password, or null if not set
     */
    public String getPassword() {
      return getField("password");
    }

    /**
     * Checks if remember-me is enabled.
     *
     * @return true if remember-me is enabled
     */
    public boolean isRememberMe() {
      return hasField("remember-me");
    }
  }

  /**
   * Logout form builder.
   */
  public static class LogoutFormBuilder extends FormBuilder {

    LogoutFormBuilder(String action) {
      super(action);
    }
  }
}
