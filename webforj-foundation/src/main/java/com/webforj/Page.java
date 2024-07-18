package com.webforj;

import com.basis.bbj.proxies.BBjClientFile;
import com.basis.bbj.proxies.BBjClientFileSystem;
import com.basis.bbj.proxies.BBjThinClient;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.concern.HasJsExecution;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.environment.ObjectTable;
import com.webforj.event.page.PageUnloadEvent;
import com.webforj.event.page.PageUnloadEventHandler;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.Assets;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.Map;

/**
 * Represents the web page open in the browser where the app is running and provides methods to
 * manipulate the page.
 *
 * @author Hyyan Abo Fakher
 * @since 23.00
 */
public final class Page implements HasJsExecution {
  private static final String FAILED_TO_DOWNLOAD_FILE = "Failed to download file.";
  private PageExecuteJsAsyncHandler executeJsAsyncHandler = null;
  private Environment environment = Environment.getCurrent();
  private EventDispatcher dispatcher = new EventDispatcher();
  private boolean isBrowserCloseEventRegistered = false;

  private Page() {}

  /**
   * Get the current page instance.
   *
   * @return the current page instance
   */
  public static Page getCurrent() {
    String key = ".page.instance";
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
   * @throws WebforjRuntimeException if failed to set the title
   */
  public Page setTitle(String title, String format, Map<String, String> placeholders)
      throws WebforjRuntimeException {
    try {
      getWebManager().setTitle(title, format, placeholders);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to set title.", e);
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
   * @throws WebforjRuntimeException if failed to set the title
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
   * @throws WebforjRuntimeException if failed to set the title
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
   * @throws WebforjRuntimeException if failed to get the title
   */
  public String getTitle() {
    try {
      return getWebManager().getTitle();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get title.", e);
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
   * @throws WebforjRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content, Map<String, String> attributes) {
    try {
      getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to set meta tag.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content, String attributes) {
    try {
      getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to set meta tag.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to set the meta tag
   */
  public Page setMeta(String name, String content) {
    try {
      getWebManager().setMeta(name, content);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to set meta tag.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to set the attribute
   */
  public Page setAttribute(String name, String value, String selector) {
    try {
      getWebManager().setAttribute(name, value, selector);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to set attribute.", e);
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
   * @throws WebforjRuntimeException if failed to set the attribute
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
   * @throws WebforjRuntimeException if failed to set the attribute
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
   * @throws WebforjRuntimeException if failed to get the attribute
   */
  public String getAttribute(String name, String selector) {
    try {
      return getWebManager().getAttribute(name, selector);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get attribute.", e);
    }
  }

  /**
   * Get an attribute from the document.
   *
   * @param name The name of the attribute
   * @return The attribute value
   * @throws WebforjRuntimeException if failed to get the attribute
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
   * @throws WebforjRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add stylesheet.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the stylesheet
   */
  public Page addStyleSheet(String url, boolean top, String attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      getWebManager().injectStyleUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add stylesheet.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the stylesheet
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
   * @throws WebforjRuntimeException if failed to add the stylesheet
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
   * @throws WebforjRuntimeException if failed to add the stylesheet
   */
  public Page addInlineStyleSheet(String css, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isContextUrl(css)) {
        css = Assets.contentOf(Assets.resolveContextUrl(css));
      }

      getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add inline stylesheet.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the stylesheet
   */
  public Page addInlineStyleSheet(String css, boolean top, String attributes) {
    try {
      if (Assets.isContextUrl(css)) {
        css = Assets.contentOf(Assets.resolveContextUrl(css));
      }

      getWebManager().injectStyle(css, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add inline stylesheet.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the stylesheet
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
   * @throws WebforjRuntimeException if failed to add the stylesheet
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
   * @throws WebforjRuntimeException if failed to add the script
   */
  public Page addJavaScript(String url, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add script.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the script
   */
  public Page addJavaScript(String url, boolean top, String attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      getWebManager().injectScriptUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add script.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the script
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
   * @throws WebforjRuntimeException if failed to add the script
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
   * @throws WebforjRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isContextUrl(script)) {
        script = Assets.contentOf(Assets.resolveContextUrl(script));
      }

      getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add inline script.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the script
   */
  public Page addInlineJavaScript(String script, boolean top, String attributes) {
    try {
      if (Assets.isContextUrl(script)) {
        script = Assets.contentOf(Assets.resolveContextUrl(script));
      }

      getWebManager().injectScript(script, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add inline script.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the script
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
   * @throws WebforjRuntimeException if failed to add the script
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
   * @throws WebforjRuntimeException if failed to add the link
   */
  public Page addLink(String url, boolean top, Map<String, String> attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add link.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the link
   */
  public Page addLink(String url, boolean top, String attributes) {
    try {
      if (Assets.isWebServerUrl(url)) {
        url = Assets.resolveWebServerUrl(url);
      }

      getWebManager().injectLinkUrl(url, top, attributes);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add link.", e); // NOSONAR
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
   * @throws WebforjRuntimeException if failed to add the link
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
   * @throws WebforjRuntimeException if failed to add the link
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
      throw new WebforjRuntimeException("Failed to execute script.", e);
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
      throw new WebforjRuntimeException("Failed to execute async script.", e);
    }
  }

  /**
   * Reload the page in the browser.
   *
   * @throws WebforjRuntimeException If fails to execute the script to reload the page
   */
  public void reload() {
    executeJsAsync("window.location.reload();");
  }

  /**
   * Sends the given InputStream to the client for download.
   *
   * @param inputStream The InputStream representing the file content
   * @param fileName The name of the file to download
   *
   * @return The current page instance
   * @since 24.02
   */
  public Page download(InputStream inputStream, String fileName) {
    Path tempFilePath = null;
    try {
      tempFilePath = Files.createTempFile(null, null);
      Files.copy(inputStream, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
      performDownload(tempFilePath.toString(), fileName);
    } catch (IOException | BBjException e) {
      throw new WebforjRuntimeException(FAILED_TO_DOWNLOAD_FILE, e);
    } finally {
      deleteTempFile(tempFilePath);
    }

    return this;
  }

  /**
   * Sends the given byte array to the client for download.
   *
   * @param content The byte array representing the file content
   * @param fileName The name of the file to download
   *
   * @return The current page instance
   * @since 24.02
   */
  public Page download(byte[] content, String fileName) {
    Path tempFilePath = null;
    try {
      tempFilePath = Files.createTempFile(null, null);
      Files.write(tempFilePath, content, StandardOpenOption.WRITE);
      performDownload(tempFilePath.toString(), fileName);
    } catch (IOException | BBjException e) {
      throw new WebforjRuntimeException(FAILED_TO_DOWNLOAD_FILE, e);
    } finally {
      deleteTempFile(tempFilePath);
    }

    return this;
  }

  /**
   * Sends the given file to the client for download.
   *
   * @param file The file to download
   * @param fileName The name of the file to download
   *
   * @return The current page instance
   * @since 24.02
   */
  public Page download(File file, String fileName) {
    try {
      performDownload(file.getAbsolutePath(), fileName);
    } catch (BBjException e) {
      throw new WebforjRuntimeException(FAILED_TO_DOWNLOAD_FILE, e);
    }

    return this;
  }

  /**
   * Sends the given file to the client for download.
   *
   * @param file The file to download
   *
   * @return The current page instance
   * @since 24.02
   */
  public Page download(File file) {
    return download(file, file.getName());
  }

  /**
   * Sends the given resource to the client for download.
   *
   * @param path The full path to a physical file or a context url. If a url is provided and starts
   *        with <code>context://</code> then the url will be resolved as a context url which points
   *        to the root of the resources folder of your application
   * @param fileName The name of the file to download
   *
   * @return The current page instance
   * @since 24.02
   */
  public Page download(String path, String fileName) {
    try {
      if (Assets.isContextUrl(path)) {
        ClassLoader classLoader = Environment.getCurrent().getClass().getClassLoader();
        InputStream is = classLoader.getResourceAsStream(Assets.resolveContextUrl(path));
        download(is, fileName);
      } else {
        performDownload(path, fileName);
      }
    } catch (BBjException e) {
      throw new WebforjRuntimeException(FAILED_TO_DOWNLOAD_FILE, e);
    }

    return this;
  }

  /**
   * Opens the given URL in a window with the given name.
   *
   * @param url The URL to open
   * @param windowName specifies a window name The special target keywords, _self, _blank (default),
   *        _parent, _top, and _unfencedTop can also be used. For more information, see
   *        <a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/open#target"> Window
   *        name</a>.
   * @param features A string containing a comma-separated list of window features in the form
   *        name=value — or for boolean features, just name. These features include options such as
   *        the window's default size and position, whether or not to open a minimal popup window,
   *        and so forth. For more information, see
   *        <a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/open#windowfeatures">
   *        Window features</a>.
   *
   * @return The current page instance
   * @since 24.10
   */
  public Page open(String url, String windowName, String features) {
    String theWindowName = windowName == null || windowName.isEmpty() ? "_blank" : windowName;
    String theFeatures = features == null ? "" : features;

    try {
      BBjThinClient thinClient = getEnvironment().getBBjAPI().getThinClient();
      thinClient.browse(url, theWindowName, theFeatures);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to open url.", e);
    }

    return this;
  }

  /**
   * Opens the given URL in a window with the given name.
   *
   * @param url The URL to open
   * @param windowName specifies a window name The special target keywords, _self, _blank (default),
   *        _parent, _top, and _unfencedTop can also be used. For more information, see
   *        <a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/open#target"> Window
   *        name</a>.
   *
   * @return The current page instance
   * @since 24.10
   */
  public Page open(String url, String windowName) {
    return open(url, windowName, "");
  }

  /**
   * Opens the given URL in a window with the given name.
   *
   * @param url The URL to open
   *
   * @return The current page instance
   * @since 24.10
   */
  public Page open(String url) {
    return open(url, null, "");
  }

  /**
   * Adds a listener to be notified when the page is unloaded by the browser.
   *
   * @param listener The listener to add
   * @return The registration object that can be used to remove the listener
   * @since 24.10
   */
  public ListenerRegistration<PageUnloadEvent> addUnloadListener(
      EventListener<PageUnloadEvent> listener) {
    if (!isBrowserCloseEventRegistered) {
      isBrowserCloseEventRegistered = true;

      try {
        BBjWebManager webManager = getEnvironment().getBBjAPI().getWebManager();
        CustomObject handler = getEnvironment().getWebforjHelper()
            .getEventProxy(new PageUnloadEventHandler(this, dispatcher), "handleEvent");
        webManager.setCallback(SysGuiEventConstants.ON_BROWSER_CLOSE, handler, "onEvent");
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to register browser close event.", e);
      }
    }

    return dispatcher.addListener(PageUnloadEvent.class, listener);
  }

  /**
   * Alias for {@link #addUnloadListener(EventListener)}.
   *
   * @param listener The listener to add
   * @return The registration object that can be used to remove the listener
   * @since 24.10
   */
  public ListenerRegistration<PageUnloadEvent> onUnload(EventListener<PageUnloadEvent> listener) {
    return addUnloadListener(listener);
  }

  private void performDownload(String sourceFilePath, String fileName) throws BBjException {
    BBjThinClient thinClient = getEnvironment().getBBjAPI().getThinClient();
    BBjClientFileSystem clientFileSystem = thinClient.getClientFileSystem();
    BBjClientFile clientFile = clientFileSystem.getClientFile(fileName);
    clientFile.copyToClient(sourceFilePath);
  }

  private void deleteTempFile(Path tempFilePath) {
    if (tempFilePath != null) {
      try {
        Files.delete(tempFilePath);
      } catch (IOException e) {
        Environment.logError("Failed to delete temp file: " + tempFilePath, e);
      }
    }
  }
}
