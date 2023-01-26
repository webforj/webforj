package org.dwcj;

import com.basis.startup.type.BBjException;

import java.util.Map;

import org.dwcj.annotations.AnnotationProcessor;
import org.dwcj.environment.namespace.*;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.exceptions.DwcException;
import org.dwcj.exceptions.DwcRuntimeException;

/**
 * This is the central class representing an app. In order to implement an app,
 * extend this class and
 * override the run() method.
 *
 */
@SuppressWarnings("java:S1610") // we want this to be abstract class, not interface
public abstract class App {

  /**
   * An enum for the default application themes
   *
   * @see App#setTheme(Theme)
   */
  public enum Theme {
    /**
     * The Light Theme
     */
    LIGHT("light"),
    /**
     * The Dark Theme
     */
    DARK("dark"),
    /**
     * The Dark Pure Theme
     */
    DARK_PURE("dark-pure"),
    /**
     * The System-set Theme
     */
    SYSTEM("system");

    private String value;

    private Theme(String value) {
      this.value = value;
    }

    /**
     *
     * @return the theme value
     */
    public String getValue() {
      return value;
    }
  }

  /**
   * Constructor
   */
  protected App() {
    preRun();
    try {
      AnnotationProcessor processor = new AnnotationProcessor();
      processor.processAppAnnotations(this, AnnotationProcessor.RunningPhase.PRE_RUN);
      run();
      processor.processAppAnnotations(this, AnnotationProcessor.RunningPhase.POST_RUN);
    } catch (DwcException e) {
      Environment.logError(e);
    }
  }

  /**
   * Set the application title
   *
   * @param title The title to set
   * @throws DwcRuntimeException
   */
  public static void setTitle(String title) {
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
   * @throws DwcRuntimeException
   */
  public static String getTitle() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getTitle();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get title.", e);
    }
  }

  /**
   * Get the registered DWC application name
   *
   * @return the application name
   * @throws DwcRuntimeException
   */
  public static String getApplicationName() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getApplicationName();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get application name.", e);
    }
  }

  /**
   * Set the application theme
   *
   * @param theme The theme to set
   * @throws DwcRuntimeException
   */
  public static void setTheme(String theme) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setTheme(theme);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set theme.", e);
    }
  }

  /**
   * Set the application theme
   *
   * @param theme The theme to set
   * @throws DwcRuntimeException
   *
   * @see Theme
   */
  public static void setTheme(Theme theme) {
    setTheme(theme.getValue());
  }

  /**
   * Get the application theme
   *
   * @return The theme
   * @throws DwcRuntimeException
   */
  public static String getTheme() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getTheme();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get theme.", e);
    }
  }

  /**
   * Set the name of the dark theme to use for the application.
   * The dark theme setting is used when the application theme is set to "system".
   *
   * @param darkTheme The dark theme to set
   * @throws DwcRuntimeException
   */
  public static void setDarkTheme(String darkTheme) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setDarkTheme(darkTheme);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set dark theme.", e);
    }
  }

  /**
   * Get the name of the dark theme
   *
   * @return The dark theme
   * @throws DwcRuntimeException
   */
  public static String getDarkTheme() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getDarkTheme();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get dark theme.", e);
    }
  }

  /**
   * Set the name of the light theme to use for the application.
   * The light theme setting is used when the application theme is set to
   * "system".
   *
   * @param lightTheme The light theme to set
   * @throws DwcRuntimeException
   */

  public static void setLightTheme(String lightTheme) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setLightTheme(lightTheme);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set light theme.", e);
    }
  }

  /**
   * Get the name of the light theme to use for the application.
   *
   * @return The light theme
   * @throws DwcRuntimeException
   */

  public static String getLightTheme() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getLightTheme();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get light theme.", e);
    }
  }

  /**
   * Set a meta tag
   *
   * @param name       The name of the meta tag
   * @param content    The content of the meta tag
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException
   */
  public static void setMeta(String name, String content, Map<String, String> attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void setMeta(String name, String content, String attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void setMeta(String name, String content) {
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
   * @throws DwcRuntimeException
   */
  public static void setAttribute(String name, String value, String selector) {
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
   * @throws DwcRuntimeException
   */
  public static void setAttribute(String name, String value) {
    setAttribute(name, value, "");
  }

  /**
   * Set an attribute on the document
   *
   * @param name The name of the attribute
   * @throws DwcRuntimeException
   */
  public static void setAttribute(String name) {
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
   * @throws DwcRuntimeException
   */
  public static String getAttribute(String name, String selector) {
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
   * @throws DwcRuntimeException
   */
  public static String getAttribute(String name) {
    return getAttribute(name, "");
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url        The URL of the stylesheet
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException
   */
  public static void addStyleSheet(String url, boolean top, Map<String, String> attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addStyleSheet(String url, boolean top, String attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addStyleSheet(String url, boolean top) {
    addStyleSheet(url, top, "");
  }

  /**
   * Inject a stylesheet into the page
   *
   * @param url The URL of the stylesheet
   * @throws DwcRuntimeException
   */
  public static void addStyleSheet(String url) {
    addStyleSheet(url, false, "");
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css        The CSS to inject
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException
   */
  public static void addInlineStyleSheet(String css, boolean top, Map<String, String> attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addInlineStyleSheet(String css, boolean top, String attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addInlineStyleSheet(String css, boolean top) {
    addInlineStyleSheet(css, top, "");
  }

  /**
   * Inject an inline stylesheet into the page
   *
   * @param css The CSS to inject
   * @throws DwcRuntimeException
   */

  public static void addInlineStyleSheet(String css) {
    addInlineStyleSheet(css, false, "");
  }

  /**
   * Inject a script into the page
   *
   * @param url        The URL of the script
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException
   */
  public static void addJavaScript(String url, boolean top, Map<String, String> attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addJavaScript(String url, boolean top, String attributes) {
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
   * @throws DwcRuntimeException
   */

  public static void addJavaScript(String url, boolean top) {
    addJavaScript(url, top, "");
  }

  /**
   * Inject a script into the page
   *
   * @param url The URL of the script
   * @throws DwcRuntimeException
   */
  public static void addJavaScript(String url) {
    addJavaScript(url, false, "");
  }

  /**
   * Inject an inline script into the page
   *
   * @param script     The script to inject
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException
   */
  public static void addInlineJavaScript(String script, boolean top, Map<String, String> attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addInlineJavaScript(String script, boolean top, String attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addInlineJavaScript(String script, boolean top) {
    addInlineJavaScript(script, top, "");
  }

  /**
   * Inject an inline script into the page
   *
   * @param script The script to inject
   * @throws DwcRuntimeException
   */
  public static void addInlineJavaScript(String script) {
    addInlineJavaScript(script, false, "");
  }

  /**
   * Inject a link into the page
   *
   * @param url        The URL of the link
   * @param top        Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcRuntimeException
   */
  public static void addLink(String url, boolean top, Map<String, String> attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addLink(String url, boolean top, String attributes) {
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
   * @throws DwcRuntimeException
   */
  public static void addLink(String url, boolean top) {
    addLink(url, top, "");
  }

  /**
   * Inject a link into the page
   *
   * @param url The URL of the link
   * @throws DwcRuntimeException
   */
  public static void addLink(String url) {
    addLink(url, false, "");
  }

  /**
   * Log a String to the browser console (console.out)
   *
   * @param output The message to log
   */
  public static void consoleLog(String output) {
    try {

      Environment.getInstance().getSysGui().executeScript("console.log(\"" + output + "\")");
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }
  /**
   * Log an Error String to the browser console (console.out)
   *
   * @param output The error message to log
   */
  public static void consoleError(String output) {
    try {

      Environment.getInstance().getSysGui().executeScript("console.error(\"" + output + "\")");
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  /**
   * Shows a message box
   *
   * @param alert The message to show
   * @return
   */
  public static int msgbox(String alert) {
    return Environment.getInstance().getDwcjHelper().msgbox(alert, 0, "");
  }

  /**
   *
   * @param alert   The message to show
   * @param options
   * @return
   */
  public static int msgbox(String alert, int options) {
    return Environment.getInstance().getDwcjHelper().msgbox(alert, options, "");
  }

  /**
   *
   * @param alert   The message to show
   * @param options
   * @param title
   * @return
   */
  public static int msgbox(String alert, int options, String title) {
    return Environment.getInstance().getDwcjHelper().msgbox(alert, options, title);
  }

  /**
   * Show or hide a busy indicator overlay
   *
   * @param busy A boolean value true=show false=hide
   */
  public static void busy(boolean busy) {
    try {
      if (busy) {
        Environment.getInstance().getBBjAPI().getBuiManager().getBusyIndicator().setText("");
      }
      Environment.getInstance().getBBjAPI().getBuiManager().getBusyIndicator().setVisible(busy);
    } catch (BBjException e) {
      // ignore
    }
  }

  /**
   * show the busy indicator with the text passed to this method
   *
   * @param busyText the text to show
   */
  public static void busy(String busyText) {
    try {
      Environment.getInstance().getBBjAPI().getBuiManager().getBusyIndicator().setText(busyText);
      Environment.getInstance().getBBjAPI().getBuiManager().getBusyIndicator().setVisible(true);
    } catch (BBjException e) {
      // ignore
    }
  }

  private void preRun() {
    Environment.getInstance().getBBjAPI().setCustomEventCallback("doTerminate", "terminate");
  }

  /**
   * Call this method to terminate your App.
   */
  public void terminate() {
    Environment.getInstance().getBBjAPI().postPriorityCustomEvent("doTerminate", null);
    cleanup();
    Environment.cleanup();
  }

  /**
   * Override this method to implement custom cleanup
   * e.g. kill all background threads that may still run
   */
  public void cleanup() {
  }

  /**
   * Override this method to implement your app behavior
   *
   * @throws DwcAppInitializeException
   */
  public abstract void run() throws DwcException;

  /**
   * Access one of the namespaces
   * @param namespaceType the type of the namespace, PRIVATE, SESSION, GROUP or GLOBAL
   * @return
   */
  public static Namespace getNamespace(Namespace.NamespaceType namespaceType) {
    switch (namespaceType) {
      case PRIVATE:
        throw new IllegalArgumentException("PRIVATE namespaces have a prefix and a name!");
      case SESSION:
        return new SessionNamespace();
      case GROUP:
        return new GroupNamespace();
      case GLOBAL:
        return new GlobalNamespace();
      default:
        throw new IllegalArgumentException("Illegal Type!");

    }
  }

  public static Namespace getNamespace(String prefix, String name, Boolean fCreateIfMissing) {
    if (prefix.isBlank() || name.isBlank())
      throw new IllegalArgumentException("You need a prefix and a name here");

    return new PrivateNamespace(prefix, name, fCreateIfMissing);

  }
}
