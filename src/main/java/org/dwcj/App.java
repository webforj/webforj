package org.dwcj;

import com.basis.startup.type.BBjException;

import java.util.Map;

import org.dwcj.annotations.AnnotationProcessor;
import org.dwcj.environment.namespace.*;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.exceptions.DwcException;

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
    LIGHT("light"),
    DARK("dark"),
    DARK_PURE("dark-pure"),
    SYSTEM("system");

    private String value;

    private Theme(String value) {
      this.value = value;
    }

    public String getValue() {
      return value;
    }
  }

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
   * @throws DwcException
   */
  public static void setTitle(String title) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setTitle(title);
    } catch (BBjException e) {
      throw new DwcException("Failed to set title.", e);
    }
  }

  /**
   * Get the application title
   * 
   * @return The title
   * @throws DwcException
   */
  public static String getTitle() throws DwcException {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getTitle();
    } catch (BBjException e) {
      throw new DwcException("Failed to get title.", e);
    }
  }

  /**
   * Get the registered DWC application name
   * 
   * @param name The name to set
   * @throws DwcException
   */
  public static String getApplicationName() throws DwcException {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getApplicationName();
    } catch (BBjException e) {
      throw new DwcException("Failed to get application name.", e);
    }
  }

  /**
   * Set the application theme
   * 
   * @param theme The theme to set
   * @throws DwcException
   */
  public static void setTheme(String theme) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setTheme(theme);
    } catch (BBjException e) {
      throw new DwcException("Failed to set theme.", e);
    }
  }

  /**
   * Set the application theme
   * 
   * @param theme The theme to set
   * @throws DwcException
   * 
   * @see Theme
   */
  public static void setTheme(Theme theme) throws DwcException {
    setTheme(theme.getValue());
  }

  /**
   * Get the application theme
   * 
   * @return The theme
   * @throws DwcException
   */
  public static String getTheme() throws DwcException {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getTheme();
    } catch (BBjException e) {
      throw new DwcException("Failed to get theme.", e);
    }
  }

  /**
   * Set the name of the dark theme to use for the application.
   * The dark theme setting is used when the application theme is set to "system".
   * 
   * @param darkTheme The dark theme to set
   * @throws DwcException
   */
  public static void setDarkTheme(String darkTheme) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setDarkTheme(darkTheme);
    } catch (BBjException e) {
      throw new DwcException("Failed to set dark theme.", e);
    }
  }

  /**
   * Get the name of the dark theme
   * 
   * @return The dark theme
   * @throws DwcException
   */
  public static String getDarkTheme() throws DwcException {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getDarkTheme();
    } catch (BBjException e) {
      throw new DwcException("Failed to get dark theme.", e);
    }
  }

  /**
   * Set the name of the light theme to use for the application.
   * The light theme setting is used when the application theme is set to
   * "system".
   * 
   * @param lightTheme The light theme to set
   * @throws DwcException
   */

  public static void setLightTheme(String lightTheme) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setLightTheme(lightTheme);
    } catch (BBjException e) {
      throw new DwcException("Failed to set light theme.", e);
    }
  }

  /**
   * Get the name of the light theme to use for the application.
   * 
   * @return The light theme
   * @throws DwcException
   */

  public static String getLightTheme() throws DwcException {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getLightTheme();
    } catch (BBjException e) {
      throw new DwcException("Failed to get light theme.", e);
    }
  }

  /**
   * Set a meta tag
   * 
   * @param name       The name of the meta tag
   * @param content    The content of the meta tag
   * @param attributes A map of attributes to set
   * @throws DwcException
   */
  public static void setMeta(String name, String content, Map<String, String> attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to set meta tag.", e); // NOSONAR
    }
  }

  /**
   * Set a meta tag
   * 
   * @param name       The name of the meta tag
   * @param content    The content of the meta tag
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcException
   */
  public static void setMeta(String name, String content, String attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to set meta tag.", e); // NOSONAR
    }
  }

  /**
   * Set a meta tag
   * 
   * @param name    The name of the meta tag
   * @param content The content of the meta tag
   * @throws DwcException
   */
  public static void setMeta(String name, String content) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content);
    } catch (BBjException e) {
      throw new DwcException("Failed to set meta tag.", e); // NOSONAR
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
   * @throws DwcException
   */
  public static void setAttribute(String name, String value, String selector) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setAttribute(name, value, selector);
    } catch (BBjException e) {
      throw new DwcException("Failed to set attribute.", e);
    }
  }

  /**
   * Set an attribute on the document
   * 
   * @param name  The name of the attribute
   * @param value The value of the attribute
   * @throws DwcException
   */
  public static void setAttribute(String name, String value) throws DwcException {
    setAttribute(name, value, "");
  }

  /**
   * Set an attribute on the document
   * 
   * @param name The name of the attribute
   * @throws DwcException
   */
  public static void setAttribute(String name) throws DwcException {
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
   * @throws DwcException
   */
  public static String getAttribute(String name, String selector) throws DwcException {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getAttribute(name, selector);
    } catch (BBjException e) {
      throw new DwcException("Failed to get attribute.", e);
    }
  }

  /**
   * Get an attribute from the document
   * 
   * @param name The name of the attribute
   * @return The attribute value
   * @throws DwcException
   */
  public static String getAttribute(String name) throws DwcException {
    return getAttribute(name, "");
  }

  /**
   * Inject a stylesheet into the page
   * 
   * @param url        The URL of the stylesheet
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcException
   */
  public static void addStyleSheet(String url, boolean top, Map<String, String> attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add stylesheet.", e); // NOSONAR
    }
  }

  /**
   * Inject a stylesheet into the page
   * 
   * @param url        The URL of the stylesheet
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcException
   */
  public static void addStyleSheet(String url, boolean top, String attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add stylesheet.", e); // NOSONAR
    }
  }

  /**
   * Inject a stylesheet into the page
   * 
   * @param url The URL of the stylesheet
   * @param top Whether to inject the stylesheet at the top of the page
   * @throws DwcException
   */
  public static void addStyleSheet(String url, boolean top) throws DwcException {
    addStyleSheet(url, top, "");
  }

  /**
   * Inject a stylesheet into the page
   * 
   * @param url The URL of the stylesheet
   * @throws DwcException
   */
  public static void addStyleSheet(String url) throws DwcException {
    addStyleSheet(url, false, "");
  }

  /**
   * Inject an inline stylesheet into the page
   * 
   * @param css        The CSS to inject
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcException
   */
  public static void addInlineStyleSheet(String css, boolean top, Map<String, String> attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add inline stylesheet.", e); // NOSONAR
    }
  }

  /**
   * Inject an inline stylesheet into the page
   * 
   * @param css        The CSS to inject
   * @param top        Whether to inject the stylesheet at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcException
   */
  public static void addInlineStyleSheet(String css, boolean top, String attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add inline stylesheet.", e); // NOSONAR
    }
  }

  /**
   * Inject an inline stylesheet into the page
   * 
   * @param css The CSS to inject
   * @param top Whether to inject the stylesheet at the top of the page
   * @throws DwcException
   */
  public static void addInlineStyleSheet(String css, boolean top) throws DwcException {
    addInlineStyleSheet(css, top, "");
  }

  /**
   * Inject an inline stylesheet into the page
   * 
   * @param css The CSS to inject
   * @throws DwcException
   */

  public static void addInlineStyleSheet(String css) throws DwcException {
    addInlineStyleSheet(css, false, "");
  }

  /**
   * Inject a script into the page
   * 
   * @param url        The URL of the script
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcException
   */
  public static void addJavaScript(String url, boolean top, Map<String, String> attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add script.", e); // NOSONAR
    }
  }

  /**
   * Inject a script into the page
   * 
   * @param url        The URL of the script
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcException
   */
  public static void addJavaScript(String url, boolean top, String attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add script.", e); // NOSONAR
    }
  }

  /**
   * Inject a script into the page
   * 
   * @param url The URL of the script
   * @param top Whether to inject the script at the top of the page
   * @throws DwcException
   */

  public static void addJavaScript(String url, boolean top) throws DwcException {
    addJavaScript(url, top, "");
  }

  /**
   * Inject a script into the page
   * 
   * @param url The URL of the script
   * @throws DwcException
   */
  public static void addJavaScript(String url) throws DwcException {
    addJavaScript(url, false, "");
  }

  /**
   * Inject an inline script into the page
   * 
   * @param script     The script to inject
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcException
   */
  public static void addInlineJavaScript(String script, boolean top, Map<String, String> attributes)
      throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add inline script.", e); // NOSONAR
    }
  }

  /**
   * Inject an inline script into the page
   * 
   * @param script     The script to inject
   * @param top        Whether to inject the script at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcException
   */
  public static void addInlineJavaScript(String script, boolean top, String attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add inline script.", e); // NOSONAR
    }
  }

  /**
   * Inject an inline script into the page
   * 
   * @param script The script to inject
   * @param top    Whether to inject the script at the top of the page
   * @throws DwcException
   */
  public static void addInlineJavaScript(String script, boolean top) throws DwcException {
    addInlineJavaScript(script, top, "");
  }

  /**
   * Inject an inline script into the page
   * 
   * @param script The script to inject
   * @throws DwcException
   */
  public static void addInlineJavaScript(String script) throws DwcException {
    addInlineJavaScript(script, false, "");
  }

  /**
   * Inject a link into the page
   * 
   * @param url        The URL of the link
   * @param top        Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set
   * @throws DwcException
   */
  public static void addLink(String url, boolean top, Map<String, String> attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add link.", e); // NOSONAR
    }
  }

  /**
   * Inject a link into the page
   * 
   * @param url        The URL of the link
   * @param top        Whether to inject the link at the top of the page
   * @param attributes A map of attributes to set (comma separated)
   * @throws DwcException
   */
  public static void addLink(String url, boolean top, String attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to add link.", e); // NOSONAR
    }
  }

  /**
   * Inject a link into the page
   * 
   * @param url The URL of the link
   * @param top Whether to inject the link at the top of the page
   * @throws DwcException
   */
  public static void addLink(String url, boolean top) throws DwcException {
    addLink(url, top, "");
  }

  /**
   * Inject a link into the page
   * 
   * @param url The URL of the link
   * @throws DwcException
   */
  public static void addLink(String url) throws DwcException {
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