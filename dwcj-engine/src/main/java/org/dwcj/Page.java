package org.dwcj;

import java.util.Map;

import org.dwcj.exceptions.DwcException;
import org.dwcj.exceptions.DwcRuntimeException;

import com.basis.startup.type.BBjException;

/**
 * Represents the web page open in the browser where the app is running
 * and provides methods to manipulate the page.
 * 
 * @author Hyyan Abo Fakher
 */
public final class Page {

  private static final Page pageInstance = new Page();

  private Page() {
  }

  /**
   * Get the current page instance
   * 
   * @return the current page instance
   */
  public static Page getInstance() {
    return pageInstance;
  }

  /**
   * Set the application title
   *
   * @param title The title to set
   * @throws DwcRuntimeException if failed to set the title
   */
  public void setTitle(String title) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setTitle(title);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set title.", e);
    }
  }

  /**
   * Get the application title
   *
   * @return The title
   * @throws DwcRuntimeException if failed to get the title
   */
  public String getTitle() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getTitle();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get title.", e);
    }
  }

  /**
   * Set a meta tag
   *
   * @param name       The name of the meta tag
   * @param content    The content of the meta tag
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException if failed to set the meta tag
   */
  public void setMeta(String name, String content, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }
  }

  /**
   * Set a meta tag
   *
   * @param name       The name of the meta tag
   * @param content    The content of the meta tag
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcRuntimeException if failed to set the meta tag
   */
  public void setMeta(String name, String content, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }
  }

  /**
   * Set a meta tag
   *
   * @param name    The name of the meta tag
   * @param content The content of the meta tag
   * @throws DwcRuntimeException if failed to set the meta tag
   */
  public void setMeta(String name, String content) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }
  }

  /**
   * Set an attribute on the document
   *
   * @param name     The name of the attribute
   * @param value    The value of the attribute
   * @param selector By default, setAttribute applies to the <a href=
   *                 "https://developer.mozilla.org/en-US/docs/Web/API/Document/documentElement">document</a>
   *                 element on
   *                 the web page. If a selector is specified, it selects a
   *                 descendant element within the document to set this attribute.
   *                 If a specified <a href=
   *                 "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector">selector</a>
   *                 doesn't return any elements, the
   *                 default document element is used.
   * @throws DwcRuntimeException if failed to set the attribute
   */
  public void setAttribute(String name, String value, String selector) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setAttribute(name, value, selector);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set attribute.", e);
    }
  }

  /**
   * Set an attribute on the document
   *
   * @param name  The name of the attribute
   * @param value The value of the attribute
   * @throws DwcRuntimeException if failed to set the attribute
   */
  public void setAttribute(String name, String value) {
    setAttribute(name, value, "");
  }

  /**
   * Set an attribute on the document
   *
   * @param name The name of the attribute
   * @throws DwcRuntimeException if failed to set the attribute
   */
  public void setAttribute(String name) {
    setAttribute(name, name, "");
  }

  /**
   * Get an attribute from the document
   *
   * @param name     The name of the attribute
   * @param selector By default, setAttribute applies to the <a href=
   *                 "https://developer.mozilla.org/en-US/docs/Web/API/Document/documentElement">document</a>
   *                 element on
   *                 the web page. If a selector is specified, it selects a
   *                 descendant element within the document to set this attribute.
   *                 If a specified <a href=
   *                 "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector">selector</a>
   *                 doesn't return any elements, the
   *                 default document element is used.
   * @return The attribute value
   * @throws DwcRuntimeException if failed to get the attribute
   */
  public String getAttribute(String name, String selector) {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getAttribute(name, selector);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get attribute.", e);
    }
  }

  /**
   * Get an attribute from the document
   *
   * @param name The name of the attribute
   * @return The attribute value
   * @throws DwcRuntimeException if failed to get the attribute
   */
  public String getAttribute(String name) {
    return getAttribute(name, "");
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url        The URL of the stylesheet
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public void addStyleSheet(String url, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add stylesheet.", e); // NOSONAR
    }
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url        The URL of the stylesheet
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public void addStyleSheet(String url, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add stylesheet.", e); // NOSONAR
    }
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url The URL of the stylesheet
   * @param top Whether to inject the stylesheet at the top of the page
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public void addStyleSheet(String url, boolean top) {
    addStyleSheet(url, top, "");
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url The URL of the stylesheet
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public void addStyleSheet(String url) {
    addStyleSheet(url, false, "");
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css        The CSS to inject
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public void addInlineStyleSheet(String css, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add inline stylesheet.", e); // NOSONAR
    }
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css        The CSS to inject
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public void addInlineStyleSheet(String css, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add inline stylesheet.", e); // NOSONAR
    }
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css The CSS to inject
   * @param top Whether to inject the stylesheet at the top of the page
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public void addInlineStyleSheet(String css, boolean top) {
    addInlineStyleSheet(css, top, "");
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css The CSS to inject
   * @throws DwcRuntimeException if failed to add the stylesheet
   */

  public void addInlineStyleSheet(String css) {
    addInlineStyleSheet(css, false, "");
  }

  /**
   * Inject a script into the page
   *
   * @param url        The URL of the script
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException if failed to add the script
   */
  public void addJavaScript(String url, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add script.", e); // NOSONAR
    }
  }

  /**
   * Inject a script into the page
   *
   * @param url        The URL of the script
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcRuntimeException if failed to add the script
   */
  public void addJavaScript(String url, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add script.", e); // NOSONAR
    }
  }

  /**
   * Inject a script into the page
   *
   * @param url The URL of the script
   * @param top Whether to inject the script at the top of the page
   * @throws DwcRuntimeException if failed to add the script
   */

  public void addJavaScript(String url, boolean top) {
    addJavaScript(url, top, "");
  }

  /**
   * Inject a script into the page
   *
   * @param url The URL of the script
   * @throws DwcRuntimeException if failed to add the script
   */
  public void addJavaScript(String url) {
    addJavaScript(url, false, "");
  }

  /**
   * Inject an inline script into the page
   *
   * @param script     The script to inject
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException if failed to add the script
   */
  public void addInlineJavaScript(String script, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add inline script.", e); // NOSONAR
    }
  }

  /**
   * Inject an inline script into the page
   *
   * @param script     The script to inject
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcRuntimeException if failed to add the script
   */
  public void addInlineJavaScript(String script, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add inline script.", e); // NOSONAR
    }
  }

  /**
   * Inject an inline script into the page
   *
   * @param script The script to inject
   * @param top    Whether to inject the script at the top of the page
   * @throws DwcRuntimeException if failed to add the script
   */
  public void addInlineJavaScript(String script, boolean top) {
    addInlineJavaScript(script, top, "");
  }

  /**
   * Inject an inline script into the page
   *
   * @param script The script to inject
   * @throws DwcRuntimeException if failed to add the script
   */
  public void addInlineJavaScript(String script) {
    addInlineJavaScript(script, false, "");
  }

  /**
   * Inject a link into the page
   *
   * @param url        The URL of the link
   * @param top        Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException if failed to add the link
   */
  public void addLink(String url, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add link.", e); // NOSONAR
    }
  }

  /**
   * Inject a link into the page
   *
   * @param url        The URL of the link
   * @param top        Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcRuntimeException if failed to add the link
   */
  public void addLink(String url, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add link.", e); // NOSONAR
    }
  }

  /**
   * Inject a link into the page
   *
   * @param url The URL of the link
   * @param top Whether to inject the link at the top of the page
   * @throws DwcRuntimeException if failed to add the link
   */
  public void addLink(String url, boolean top) {
    addLink(url, top, "");
  }

  /**
   * Inject a link into the page
   *
   * @param url The URL of the link
   * @throws DwcRuntimeException if failed to add the link
   */
  public void addLink(String url) {
    addLink(url, false, "");
  }

  /**
   * Execute a script in the browser and return the result
   * 
   * @param script The script to execute
   * @return The result of the script
   * @throws DwcException If dwcj fails to execute the script
   */
  public Object executeJs(String script) throws DwcException {
    try {
      return Environment.getInstance().getSysGui().executeScript(script);
    } catch (BBjException e) {
      throw new DwcException("Failed to execute script.", e); // NOSONAR
    }
  }

  /**
   * Execute a script in the browser without waiting for the result
   * 
   * @param script The script to execute
   * @return The result of the script
   * @throws DwcException If dwcj fails to execute the script
   */
  public void executeAsyncJs(String script) throws DwcException {
    try {
      Environment.getInstance().getSysGui().executeAsyncScript(script);
    } catch (BBjException e) {
      throw new DwcException("Failed to execute async script.", e); // NOSONAR
    }
  }
}