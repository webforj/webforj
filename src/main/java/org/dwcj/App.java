package org.dwcj;

import com.basis.startup.type.BBjException;

import java.util.HashMap;

import org.dwcj.annotations.AppDarkTheme;
import org.dwcj.annotations.AppMeta;
import org.dwcj.annotations.AppTheme;
import org.dwcj.annotations.AppTitle;
import org.dwcj.annotations.MetaAttribute;
import org.dwcj.environment.namespace.*;
import org.dwcj.exceptions.DwcAnnotationException;
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

  protected App() {
    preRun();
    try {
      this.processAnnotations();
      run();
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
  public void setTitle(String title) throws DwcException {
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
  public String getTitle() throws DwcException {
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
  public String getApplicationName() throws DwcException {
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
  public void setTheme(String theme) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setTheme(theme);
    } catch (BBjException e) {
      throw new DwcException("Failed to set theme.", e);
    }
  }

  /**
   * Get the application theme
   * 
   * @return The theme
   * @throws DwcException
   */
  public String getTheme() throws DwcException {
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
  public void setDarkTheme(String darkTheme) throws DwcException {
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
  public String getDarkTheme() throws DwcException {
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

  public void setLightTheme(String lightTheme) throws DwcException {
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

  public String getLightTheme() throws DwcException {
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
  public void setMeta(String name, String content, HashMap<String, String> attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to set meta tag.", e);
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
  public void setMeta(String name, String content, String attributes) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content, attributes);
    } catch (BBjException e) {
      throw new DwcException("Failed to set meta tag.", e);
    }
  }

  /**
   * Set a meta tag
   * 
   * @param name    The name of the meta tag
   * @param content The content of the meta tag
   * @throws DwcException
   */
  public void setMeta(String name, String content) throws DwcException {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setMeta(name, content);
    } catch (BBjException e) {
      throw new DwcException("Failed to set meta tag.", e);
    }
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

  /**
   * Process all app level annotations
   * 
   * @throws DwcAnnotationException
   */
  private void processAnnotations() throws DwcAnnotationException {
    // process AppTitle annotation
    AppTitle appTitle = this.getClass().getAnnotation(AppTitle.class);
    if (appTitle != null) {
      try {
        setTitle(appTitle.value());
      } catch (DwcException e) {
        throw new DwcAnnotationException("Failed to process AppTitle annotation.", e);
      }
    }

    // process AppDarkTheme annotation
    AppDarkTheme appDarkTheme = this.getClass().getAnnotation(AppDarkTheme.class);
    if (appDarkTheme != null) {
      try {
        setDarkTheme(appDarkTheme.value());
      } catch (DwcException e) {
        throw new DwcAnnotationException("Failed to process AppDarkTheme annotation.", e);
      }
    }

    // process AppLightTheme annotation
    AppTheme appLightTheme = this.getClass().getAnnotation(AppTheme.class);
    if (appLightTheme != null) {
      try {
        setLightTheme(appLightTheme.value());
      } catch (DwcException e) {
        throw new DwcAnnotationException("Failed to process AppTheme annotation.", e);
      }
    }

    // process AppTheme annotation
    AppTheme appTheme = this.getClass().getAnnotation(AppTheme.class);
    if (appTheme != null) {
      try {
        setTheme(appTheme.value());
      } catch (DwcException e) {
        throw new DwcAnnotationException("Failed to process AppTheme annotation.", e);
      }
    }

    // process AppMeta annotation
    AppMeta[] appMeta = this.getClass().getAnnotationsByType(AppMeta.class);
    if (appMeta != null) {
      for (AppMeta meta : appMeta) {
        try {
          HashMap<String, String> attributes = new HashMap<>();
          for (MetaAttribute attribute : meta.attributes()) {
            attributes.put(attribute.name(), attribute.value());
          }

          setMeta(meta.name(), meta.content(), attributes);
        } catch (DwcException e) {
          throw new DwcAnnotationException("Failed to process AppMeta annotation.", e);
        }
      }
    }
  }
}