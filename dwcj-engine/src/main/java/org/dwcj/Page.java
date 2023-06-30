package org.dwcj;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.dwcj.environment.ObjectTable;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.Assets;

/**
 * Represents the web page open in the browser where the app is running and provides methods to
 * manipulate the page.
 *
 * @author Hyyan Abo Fakher
 */
public final class Page {

  /**
   * The type of groups available when setting the user properties.
   */
  public enum PropertyGroup {

    /**
     * Maps to browser cookies with a 30-day expiration.
     */
    USER_PROPERTIES_COOKIES(0),

    /**
     * Maps to browser local storage.
     */
    USER_PROPERTIES_STORAGE(1),

    /**
     * Maps to browser session storage.
     */
    USER_PROPERTIES_SESSION(-1);

    private long value;

    private PropertyGroup(long value) {
      this.value = value;
    }

    /**
     * Get the value of the enum.
     *
     * @return the value of the enum
     */
    public long getValue() {
      return value;
    }


    /**
     * Get the enum from the value.
     *
     * @param value the value of the enum
     * @return the enum
     */
    public static PropertyGroup fromValue(long value) {
      for (PropertyGroup group : PropertyGroup.values()) {
        if (group.getValue() == value) {
          return group;
        }
      }

      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  /**
   * Controls the set of domains that can read a given cookie. For more information, see Google's
   * notes for Chrome 80+.
   */
  public enum PropertySamesite {

    /**
     * The default SameSite behavior is set to Lax.
     */
    SAME_SITE_DEFAULT(""),

    /**
     * This value provides a balance between security and usability. The cookie will not be sent in
     * cross-site requests that are initiated by "top-level" navigation (e.g., clicking a link), but
     * it will be sent in "subsequent" requests that occur within the context of the site.
     */
    SAME_SITE_LAX("Lax"),

    /**
     * This value allows the cookie to be sent in cross-site requests. However, there is an
     * additional requirement for secure transmission. It means that the cookie will only be sent if
     * the request is made over HTTPS.
     */
    SAME_SITE_NONE("None"),

    /**
     * With this value, the cookie will only be sent in requests that originate from the same site
     * (site for which the cookie was set). It will not be sent in any cross-site context, including
     * when navigating from an external site or using subdomain links.
     */
    SAME_SITE_STRICT("Strict");

    private String value;

    private PropertySamesite(String value) {
      this.value = value;
    }

    /**
     * Get the value of the enum.
     *
     * @return the value of the enum
     */
    public String getValue() {
      return this.value;
    }

    /**
     * Get the enum from the value.
     *
     * @param value the value of the enum
     * @return the enum
     */
    public static PropertySamesite fromValue(String value) {
      for (PropertySamesite samesite : PropertySamesite.values()) {
        if (samesite.getValue().equals(value)) {
          return samesite;
        }
      }

      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return this.value;
    }
  }

  BBjThinClient thinClient;


  private Page() {
    try {
      thinClient = Environment.getInstance().getBBjAPI().getThinClient();
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to instantiate Page.", e);
    }
  }

  /*
   * only for testing
   */
  Page(BBjThinClient client) {
    this.thinClient = client;
  }

  /**
   * Get the current page instance.
   *
   * @return the current page instance
   */
  public static Page getCurrent() {
    String key = "dwcj.page.instance";
    if (ObjectTable.contains(key)) {
      return (Page) ObjectTable.get(key);
    }

    Page instance = new Page();
    ObjectTable.put(key, instance);

    return instance;
  }

  /**
   * Set the application title.
   *
   * @param title The title to set
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the title
   */
  public Page setTitle(String title) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setTitle(title);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set title.", e);
    }

    return this;
  }

  /**
   * Get the application title.
   *
   * @return The title
   * @throws DwcjRuntimeException if failed to get the title
   */
  public String getTitle() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getTitle();
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to get title.", e);
    }
  }

  /**
   * Set a meta tag.
   *
   * @param name The name of the meta tag
   * @param content The content of the meta tag
   * @param attributes A map of attributes to set
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Set a meta tag.
   *
   * @param name The name of the meta tag
   * @param content The content of the meta tag
   * @param attributes A map of attributes to set (comma separated)
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Set a meta tag.
   *
   * @param name The name of the meta tag
   * @param content The content of the meta tag
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Set an attribute on the document.
   *
   * @param name The name of the attribute
   * @param value The value of the attribute
   * @param selector By default, setAttribute applies to the <a href=
   *        "https://developer.mozilla.org/en-US/docs/Web/API/Document/documentElement">document</a>
   *        element on the web page. If a selector is specified, it selects a descendant element
   *        within the document to set this attribute. If a specified <a href=
   *        "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector">selector</a>
   *        doesn't return any elements, the default document element is used.
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the attribute
   */
  public Page setAttribute(String name, String value, String selector) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setAttribute(name, value, selector);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set attribute.", e);
    }

    return this;
  }

  /**
   * Set an attribute on the document.
   *
   * @param name The name of the attribute
   * @param value The value of the attribute
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the attribute
   */
  public Page setAttribute(String name, String value) {
    return setAttribute(name, value, "");
  }

  /**
   * Set an attribute on the document.
   *
   * @param name The name of the attribute
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the attribute
   */
  public Page setAttribute(String name) {
    return setAttribute(name, name, "");
  }

  /**
   * Get an attribute from the document.
   *
   * @param name The name of the attribute
   * @param selector By default, setAttribute applies to the <a href=
   *        "https://developer.mozilla.org/en-US/docs/Web/API/Document/documentElement">document</a>
   *        element on the web page. If a selector is specified, it selects a descendant element
   *        within the document to set this attribute. If a specified <a href=
   *        "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector">selector</a>
   *        doesn't return any elements, the default document element is used.
   * @return The attribute value
   * @throws DwcjRuntimeException if failed to get the attribute
   */
  public String getAttribute(String name, String selector) {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getAttribute(name, selector);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to get attribute.", e);
    }
  }

  /**
   * Get an attribute from the document.
   *
   * @param name The name of the attribute
   * @return The attribute value
   * @throws DwcjRuntimeException if failed to get the attribute
   */
  public String getAttribute(String name) {
    return getAttribute(name, "");
  }

  /**
   * Inject a stylesheet into the page.
   *
   * @param url The URL of the stylesheet. The url will be resolved as a web server url if it starts
   *        with the <code>webserver://</code>
   * @param top Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add stylesheet.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a stylesheet into the page.
   *
   * @param url The URL of the stylesheet. The url will be resolved as a web server url if it starts
   *        with the <code>webserver://</code>
   * @param top Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url, boolean top, String attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add stylesheet.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a stylesheet into the page.
   *
   * @param url The URL of the stylesheet. The url will be resolved as a web server url if it starts
   *        with the <code>webserver://</code>
   * @param top Whether to inject the stylesheet at the top of the page
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url, boolean top) {
    return addStyleSheet(url, top, "");
  }

  /**
   * Inject a stylesheet into the page.
   *
   * @param url The URL of the stylesheet. The url will be resolved as a web server url if it starts
   *        with the <code>webserver://</code>
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url) {
    return addStyleSheet(url, false, "");
  }

  /**
   * Inject an inline stylesheet into the page.
   *
   * @param css The CSS to inject. If a url is provided and starts with <code>context://</code> then
   *        the url will be resolved as a context url which points to the root of the resources
   *        folder of your application
   * @param top Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the stylesheet
   */
  public Page addInlineStyleSheet(String css, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isContextUrl(css)) {
        css = Assets.contentOf(Assets.resolveContextUrl(css));
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add inline stylesheet.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject an inline stylesheet into the page.
   *
   * @param css The CSS to inject. If a url is provided and starts with <code>context://</code> then
   *        the url will be resolved as a context url which points to the root of the resources
   *        folder of your application
   * @param top Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the stylesheet
   */
  public Page addInlineStyleSheet(String css, boolean top, String attributes) {
    try {
      if (Assets.isContextUrl(css)) {
        css = Assets.contentOf(Assets.resolveContextUrl(css));
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add inline stylesheet.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject an inline stylesheet into the page.
   *
   * @param css The CSS to inject. If a url is provided and starts with <code>context://</code> then
   *        the url will be resolved as a context url which points to the root of the resources
   *        folder of your application
   * @param top Whether to inject the stylesheet at the top of the page
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the stylesheet
   */
  public Page addInlineStyleSheet(String css, boolean top) {
    return addInlineStyleSheet(css, top, "");
  }

  /**
   * Inject an inline stylesheet into the page.
   *
   * @param css The CSS to inject. If a url is provided and starts with <code>context://</code> then
   *        the url will be resolved as a context url which points to the root of the resources
   *        folder of your application
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the stylesheet
   */

  public Page addInlineStyleSheet(String css) {
    return addInlineStyleSheet(css, false, "");
  }

  /**
   * Inject a script into the page.
   *
   * @param url The URL of the script. The url will be resolved as a web server url if it starts
   *        with the <code>webserver://</code>
   * @param top Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the script
   */
  public Page addJavaScript(String url, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a script into the page.
   *
   * @param url The URL of the script. The url will be resolved as a web server url if it starts
   *        with the <code>webserver://</code>
   * @param top Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the script
   */
  public Page addJavaScript(String url, boolean top, String attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a script into the page.
   *
   * @param url The URL of the script.The url will be resolved as a web server url if it starts with
   *        the <code>webserver://</code>
   * @param top Whether to inject the script at the top of the page
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the script
   */

  public Page addJavaScript(String url, boolean top) {
    return addJavaScript(url, top, "");
  }

  /**
   * Inject a script into the page.
   *
   * @param url The URL of the script. The url will be resolved as a web server url if it starts
   *        with the <code>webserver://</code>
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the script
   */
  public Page addJavaScript(String url) {
    return addJavaScript(url, false, "");
  }

  /**
   * Inject an inline script into the page.
   *
   * @param script The script to inject. If a url is provided and starts with
   *        <code>context://</code> then the url will be resolved as a context url which points to
   *        the root of the resources folder of your application
   * @param top Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isContextUrl(script)) {
        script = Assets.contentOf(Assets.resolveContextUrl(script));
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add inline script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject an inline script into the page.
   *
   * @param script The script to inject. If a url is provided and starts with
   *        <code>context://</code> then the url will be resolved as a context url which points to
   *        the root of the resources folder of your application
   * @param top Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script, boolean top, String attributes) {
    try {
      if (Assets.isContextUrl(script)) {
        script = Assets.contentOf(Assets.resolveContextUrl(script));
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add inline script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject an inline script into the page.
   *
   * @param script The script to inject. If a url is provided and starts with
   *        <code>context://</code> then the url will be resolved as a context url which points to
   *        the root of the resources folder of your application
   * @param top Whether to inject the script at the top of the page
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script, boolean top) {
    return addInlineJavaScript(script, top, "");
  }

  /**
   * Inject an inline script into the page.
   *
   * @param script The script to inject. If a url is provided and starts with
   *        <code>context://</code> then the url will be resolved as a context url which points to
   *        the root of the resources folder of your application
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script) {
    return addInlineJavaScript(script, false, "");
  }

  /**
   * Inject a link into the page.
   *
   * @param url The URL of the link. The url will be resolved as a web server url if it starts with
   *        the <code>webserver://</code>
   * @param top Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the link
   */
  public Page addLink(String url, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add link.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a link into the page.
   *
   * @param url The URL of the link. The url will be resolved as a web server url if it starts with
   *        the <code>webserver://</code>
   * @param top Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the link
   */
  public Page addLink(String url, boolean top, String attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      Environment.getInstance().getBBjAPI().getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add link.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a link into the page.
   *
   * @param url The URL of the link. The url will be resolved as a web server url if it starts with
   *        the <code>webserver://</code>
   * @param top Whether to inject the link at the top of the page
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the link
   */
  public Page addLink(String url, boolean top) {
    return addLink(url, top, "");
  }

  /**
   * Inject a link into the page.
   *
   * @param url The URL of the link.The url will be resolved as a web server url if it starts with
   *        the <code>webserver://</code>
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to add the link
   */
  public Page addLink(String url) {
    return addLink(url, false, "");
  }

  /**
   * Execute a script in the browser and return the result.
   *
   * @param script The script to execute
   * @return The result of the script
   * @throws DwcjRuntimeException If dwcj fails to execute the script
   */
  public Object executeJs(String script) {
    try {
      return Environment.getInstance().getSysGui().executeScript(script);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to execute script.", e); // NOSONAR
    }
  }

  /**
   * Execute a script in the browser without waiting for the result.
   *
   * @param script The script to execute
   *
   * @return The current page instance
   * @throws DwcjRuntimeException If dwcj fails to execute the script
   */
  public Page executeAsyncJs(String script) {
    try {
      Environment.getInstance().getSysGui().executeAsyncScript(script);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to execute async script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Reload the page in the browser.
   *
   * @return The current page instance
   * @throws DwcjRuntimeException If dwcj fails to execute the script to reload the page
   */
  public Page reload() {
    return executeAsyncJs("window.location.reload();");
  }

  /**
   * Stores a key/value pair in a specified client-side property group.
   *
   * @param group Desired property group
   * @param samesite Controls the set of domains that can read a given cookie
   * @param key The key to access your stored data
   * @param value The value to be stored
   * @return The current page instance
   */
  public Page setUserProperty(PropertyGroup group, PropertySamesite samesite, String key,
      String value) {
    try {
      thinClient.setUserProperty(group.getValue(), samesite.getValue(), key, value);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set user property.", e); // NOSONAR
    }
    return this;
  }

  /**
   * Stores a key/value pair in a specified client-side property group.
   *
   * @param group Desired property group
   * @param key The key to access your stored data
   * @param value The value to be stored
   * @return The current page instance
   */
  public Page setUserProperty(PropertyGroup group, String key, String value) {
    try {
      thinClient.setUserProperty(group.getValue(), key, value);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set user property.", e); // NOSONAR
    }
    return this;
  }

  /**
   * Stores a key/value pair in a specified client-side property group.
   *
   * @param key the key to access your stored data
   * @param value the value to be stored
   * @return The current page instance
   */
  public Page setUserProperty(String key, String value) {
    try {
      thinClient.setUserProperty(key, value);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set user property.", e); // NOSONAR
    }
    return this;
  }

  /**
   * Returns the value of a client-side property for a specified key within a specified client-side
   * property group.
   *
   * @param group Desired property group
   * @param key The key to access your stored data
   * @return The user property, or null in case of an error
   */
  public String getUserProperty(PropertyGroup group, String key) {
    try {
      return thinClient.getUserProperty(group.getValue(), key);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Returns the value of a client-side property for a specified key within a specified client-side
   * property group.
   *
   * @param key The key of the property
   * @return The user property, or null in case of an error
   */
  public String getUserProperty(String key) {
    try {
      return thinClient.getUserProperty(key);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Clears all user properties in all groups.
   *
   * @return The current page instance
   */
  public Page clearUserProperties() {
    try {
      thinClient.clearUserProperties();
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to clear user properties.", e); // NOSONAR
    }
    return this;
  }

  /**
   * Clears all user properties for the given group.
   *
   * @param group The property group which will be cleared.
   * @return The current page instance
   */
  public Page clearUserProperties(PropertyGroup group) {
    try {
      thinClient.clearUserProperties(group.getValue());
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to clear user properties.", e); // NOSONAR
    }
    return this;
  }

  /**
   * Returns all user properties.
   *
   * @return Map with all user properties.
   */
  public Map<String, String> getUserProperties() {
    try {
      return thinClient.getUserProperties();
    } catch (Exception e) {
      return new HashMap<>();
    }
  }

  /**
   * Returns the stored user properties for the given keys.
   *
   * @param keys A collection of keys.
   * @return A map with all keys and values.
   */
  public Map<String, String> getUserProperties(Collection<String> keys) {
    try {
      return thinClient.getUserProperties(keys);
    } catch (Exception e) {
      return new HashMap<>();
    }
  }

  /**
   * Returns the stored user properties for the given group.
   *
   * @param group The propertie group which is queried.
   * @return A map with all keys and values
   */
  public Map<String, String> getUserProperties(PropertyGroup group) {
    try {
      return thinClient.getUserProperties(group.getValue());
    } catch (Exception e) {
      return new HashMap<>();
    }
  }

  /**
   * Returns the stored user properties for the given group and keys.
   *
   * @param group The group which is searched
   * @param keys The collection of keys
   * @return A map with all found keys and values
   */
  public Map<String, String> getUserProperties(PropertyGroup group, Collection<String> keys) {
    try {
      return thinClient.getUserProperties(group.getValue(), keys);
    } catch (Exception e) {
      return new HashMap<>();
    }
  }

  /**
   * Sets all user properties.
   *
   * @param properties A map of key and value pairs.
   * @return The current page instance
   */
  public Page setUserProperties(Map<String, String> properties) {
    try {
      thinClient.setUserProperties(properties);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set user properties.", e); // NOSONAR
    }
    return this;
  }

  /**
   * Sets all user properties to the given group.
   *
   * @param group The group the properties are saved in
   * @param properties The map of properties to save
   * @return The current page instance
   */
  public Page setUserProperties(PropertyGroup group, Map<String, String> properties) {
    try {
      thinClient.setUserProperties(group.getValue(), properties);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set user properties.", e); // NOSONAR
    }
    return this;
  }

  /**
   * Saves the Map to the given group.
   *
   * @param group The group where the properties should be stored
   * @param samesite Samesite to control the set of domains that can read a given cookie
   * @param properties The map with keys and values
   * @return The current page instance
   */
  public Page setUserProperties(PropertyGroup group, PropertySamesite samesite,
      Map<String, String> properties) {
    try {
      thinClient.setUserProperties(group.getValue(), samesite.getValue(), properties);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set user properties.", e); // NOSONAR
    }
    return this;
  }
}
