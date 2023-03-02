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
   * @return The current page instance
   * @throws DwcRuntimeException if failed to set the title
   */
  public Page setTitle(String title) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setTitle(title);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set title.", e);
    }

    return this;
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
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Set a meta tag
   *
   * @param name       The name of the meta tag
   * @param content    The content of the meta tag
   * @param attributes A map of attributes to set (comma separated)
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Set a meta tag
   *
   * @param name    The name of the meta tag
   * @param content The content of the meta tag
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set meta tag.", e); // NOSONAR
    }

    return this;
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
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to set the attribute
   */
  public Page setAttribute(String name, String value, String selector) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setAttribute(name, value, selector);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set attribute.", e);
    }

    return this;
  }

  /**
   * Set an attribute on the document
   *
   * @param name  The name of the attribute
   * @param value The value of the attribute
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to set the attribute
   */
  public Page setAttribute(String name, String value) {
    return setAttribute(name, value, "");
  }

  /**
   * Set an attribute on the document
   *
   * @param name The name of the attribute
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to set the attribute
   */
  public Page setAttribute(String name) {
    return setAttribute(name, name, "");
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
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add stylesheet.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url        The URL of the stylesheet
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add stylesheet.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url The URL of the stylesheet
   * @param top Whether to inject the stylesheet at the top of the page
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url, boolean top) {
    return addStyleSheet(url, top, "");
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url The URL of the stylesheet
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url) {
    return addStyleSheet(url, false, "");
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css        The CSS to inject
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public Page addInlineStyleSheet(String css, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add inline stylesheet.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css        The CSS to inject
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public Page addInlineStyleSheet(String css, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add inline stylesheet.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css The CSS to inject
   * @param top Whether to inject the stylesheet at the top of the page
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the stylesheet
   */
  public Page addInlineStyleSheet(String css, boolean top) {
    return addInlineStyleSheet(css, top, "");
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css The CSS to inject
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the stylesheet
   */

  public Page addInlineStyleSheet(String css) {
    return addInlineStyleSheet(css, false, "");
  }

  /**
   * Inject a script into the page
   *
   * @param url        The URL of the script
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the script
   */
  public Page addJavaScript(String url, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a script into the page
   *
   * @param url        The URL of the script
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the script
   */
  public Page addJavaScript(String url, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a script into the page
   *
   * @param url The URL of the script
   * @param top Whether to inject the script at the top of the page
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the script
   */

  public Page addJavaScript(String url, boolean top) {
    return addJavaScript(url, top, "");
  }

  /**
   * Inject a script into the page
   *
   * @param url The URL of the script
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the script
   */
  public Page addJavaScript(String url) {
    return addJavaScript(url, false, "");
  }

  /**
   * Inject an inline script into the page
   *
   * @param script     The script to inject
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add inline script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject an inline script into the page
   *
   * @param script     The script to inject
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add inline script.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject an inline script into the page
   *
   * @param script The script to inject
   * @param top    Whether to inject the script at the top of the page
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script, boolean top) {
    return addInlineJavaScript(script, top, "");
  }

  /**
   * Inject an inline script into the page
   *
   * @param script The script to inject
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script) {
    return addInlineJavaScript(script, false, "");
  }

  /**
   * Inject a link into the page
   *
   * @param url        The URL of the link
   * @param top        Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the link
   */
  public Page addLink(String url, boolean top, Map<String, String> attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add link.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a link into the page
   *
   * @param url        The URL of the link
   * @param top        Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the link
   */
  public Page addLink(String url, boolean top, String attributes) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to add link.", e); // NOSONAR
    }

    return this;
  }

  /**
   * Inject a link into the page
   *
   * @param url The URL of the link
   * @param top Whether to inject the link at the top of the page
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the link
   */
  public Page addLink(String url, boolean top) {
    return addLink(url, top, "");
  }

  /**
   * Inject a link into the page
   *
   * @param url The URL of the link
   * 
   * @return The current page instance
   * @throws DwcRuntimeException if failed to add the link
   */
  public Page addLink(String url) {
    return addLink(url, false, "");
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
   * 
   * @return The current page instance
   * @throws DwcException If dwcj fails to execute the script
   */
  public Page executeAsyncJs(String script) throws DwcException {
    try {
      Environment.getInstance().getSysGui().executeAsyncScript(script);
    } catch (BBjException e) {
      throw new DwcException("Failed to execute async script.", e); // NOSONAR
    }

    return this;
  }
}