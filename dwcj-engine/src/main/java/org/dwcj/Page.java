package org.dwcj;

import com.basis.bbj.proxies.BBjWebManager;
import com.basis.startup.type.BBjException;
import java.util.HashMap;
import java.util.Map;
import org.dwcj.concern.HasJsExecution;
import org.dwcj.environment.ObjectTable;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.Assets;

/**
 * Represents the web page open in the browser where the app is running and provides methods to
 * manipulate the page.
 *
 * @author Hyyan Abo Fakher
 */
public final class Page implements HasJsExecution {

  private PageExecuteJsAsyncHandler executeJsAsyncHandler = null;
  private Environment environment = Environment.getCurrent();

  private Page() {}

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
   * Get the current environment instance.
   *
   * @return the current environment instance
   */
  Environment getEnvironment() {
    return environment;
  }

  /**
   * Get the web manager.
   *
   * @return The web manager
   * @throws BBjException if failed to get the web manager
   */
  BBjWebManager getWebManager() throws BBjException {
    return getEnvironment().getBBjAPI().getWebManager();
  }

  /**
   * Set the application title.
   *
   * <p>
   * The title format can contain the following predefined placeholders:
   * <ul>
   * <li><code>{BrowserTitle}</code>: The title of the browser tab</li>
   * <li><code>{WindowTitle}</code>: The title of current window</li>
   * </ul>
   *
   * or any of the passed placeholders. The placeholders are replaced with the values of the of the
   * passed placeholders map. For example:
   *
   * <pre>
   * setTitle("My App", "{BrowserTitle} - {WindowTitle}::{company}", Map.of("company", "My
   * Company"));
   * </pre>
   * </p>
   *
   * @param title The title to set
   * @param format The format of the title.
   * @param placeholders The parameters to use in the format
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the title
   */
  public Page setTitle(String title, String format, Map<String, String> placeholders)
      throws DwcjRuntimeException {
    try {
      getWebManager().setTitle(title, format, placeholders);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set title.", e);
    }

    return this;
  }

  /**
   * Set the application title.
   *
   * @param title The title to set
   * @param format The format of the title
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the title
   *
   * @see #setTitle(String, String, Map)
   */
  public Page setTitle(String title, String format) {
    return setTitle(title, format, new HashMap<>());
  }

  /**
   * Set the application title.
   *
   * @param title The title to set
   *
   * @return The current page instance
   * @throws DwcjRuntimeException if failed to set the title
   *
   * @see #setTitle(String, String, Map)
   */
  public Page setTitle(String title) {
    return setTitle(title, "{BrowserTitle}");
  }

  /**
   * Get the application title.
   *
   * @return The title
   * @throws DwcjRuntimeException if failed to get the title
   */
  public String getTitle() {
    try {
      return getWebManager().getTitle();
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
      getWebManager().setMeta(name, content, attributes);
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
      getWebManager().setMeta(name, content, attributes);
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
      getWebManager().setMeta(name, content);
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
      getWebManager().setAttribute(name, value, selector);
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
      return getWebManager().getAttribute(name, selector);
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

      getWebManager().injectStyleUrl(url, top, attributes);
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

      getWebManager().injectStyleUrl(url, top, attributes);
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

      getWebManager().injectStyle(css, top, attributes);
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

      getWebManager().injectStyle(css, top, attributes);
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

      getWebManager().injectScriptUrl(url, top, attributes);
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

      getWebManager().injectScriptUrl(url, top, attributes);
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

      getWebManager().injectScript(script, top, attributes);
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

      getWebManager().injectScript(script, top, attributes);
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

      getWebManager().injectLinkUrl(url, top, attributes);
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

      getWebManager().injectLinkUrl(url, top, attributes);
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
   * {@inheritDoc}
   */
  @Override
  public Object executeJs(String script) {
    try {
      return getWebManager().executeScript(script);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to execute script.", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PendingResult<Object> executeJsAsync(String js) {
    try {
      // create the handler if it doesn't exist
      if (executeJsAsyncHandler == null) {
        executeJsAsyncHandler = new PageExecuteJsAsyncHandler(getEnvironment());
        executeJsAsyncHandler.register();
      }

      PendingResult<Object> result = new PendingResult<>();
      int index = getWebManager().executeAsyncScript(js);
      executeJsAsyncHandler.getPendingResults().put(index, result);

      return result;
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to execute async script.", e);
    }
  }

  /**
   * Reload the page in the browser.
   *
   * @throws DwcjRuntimeException If dwcj fails to execute the script to reload the page
   */
  public void reload() {
    executeJsAsync("window.location.reload();");
  }
}
