package org.dwcj.annotation;

import java.util.Arrays;
import java.util.HashMap;
import org.dwcj.App;
import org.dwcj.Page;
import org.dwcj.component.Component;
import org.dwcj.environment.ObjectTable;
import org.dwcj.environment.StringTable;

/**
 * Annotation processor for the application and controls annotations.
 *
 * @author Hyyan Abo Fakher
 */
public final class AnnotationProcessor {
  /**
   * The running phase of the application.
   */
  public enum RunningPhase {
    /**
     * The application is running before the run method.
     */
    PRE_RUN,
    /**
     * The application is running after the run method.
     */
    POST_RUN
  }

  Page getPage() {
    return App.getPage();
  }

  /**
   * Process the annotations of the application.
   *
   * @param app The application to process
   * @param phase The phase of the application
   */
  public void processAppAnnotations(App app, RunningPhase phase) {
    if (phase == RunningPhase.PRE_RUN) {
      processConfiguration(app, RunningPhase.PRE_RUN);
      processAppAttribute(app);
      processAppMeta(app);
      processLink(app);
      processStyleSheet(app);
      processInlineStyleSheet(app);
      processJavaScript(app);
      processInlineJavaScript(app);
      processAppDarkTheme(app);
      processAppLightTheme(app);
      processAppTheme(app);
    }

    if (phase == RunningPhase.POST_RUN) {
      // we don't need panels to override the app title
      processAppTitle(app);
    }
  }

  /**
   * Process the annotations of the control.
   *
   * @param control The control to process
   */
  public void processControlAnnotations(Component control) {
    processConfiguration(control, RunningPhase.POST_RUN);
    processLink(control);
    processStyleSheet(control);
    processInlineStyleSheet(control);
    processJavaScript(control);
    processInlineJavaScript(control);
  }

  /**
   * Process the AppTitle annotation.
   *
   * @param clazz The class to process
   */
  private void processAppTitle(Object clazz) {
    AppTitle appTitle = clazz.getClass().getAnnotation(AppTitle.class);
    if (appTitle != null) {
      getPage().setTitle(appTitle.value(), appTitle.format());
    }
  }

  /**
   * Process the AppAttribute annotation.
   *
   * @param clazz The class to process
   */
  private void processAppAttribute(Object clazz) {
    AppAttribute[] appAttributes = clazz.getClass().getAnnotationsByType(AppAttribute.class);
    if (appAttributes != null) {
      for (AppAttribute appAttribute : appAttributes) {
        getPage().setAttribute(appAttribute.name(), appAttribute.value(), appAttribute.selector());
      }
    }
  }

  /**
   * Process the AppDarkTheme annotation.
   *
   * @param clazz The class to process
   */
  private void processAppDarkTheme(Object clazz) {
    AppDarkTheme appDarkTheme = clazz.getClass().getAnnotation(AppDarkTheme.class);
    if (appDarkTheme != null) {
      App.setDarkTheme(appDarkTheme.value());
    }
  }

  /**
   * Process the AppLightTheme annotation.
   *
   * @param clazz The class to process
   */
  private void processAppLightTheme(Object clazz) {
    AppLightTheme appLightTheme = clazz.getClass().getAnnotation(AppLightTheme.class);
    if (appLightTheme != null) {
      App.setLightTheme(appLightTheme.value());
    }
  }

  /**
   * Process the AppTheme annotation.
   *
   * @param clazz The class to process
   */
  private void processAppTheme(Object clazz) {
    AppTheme appTheme = clazz.getClass().getAnnotation(AppTheme.class);
    if (appTheme != null) {
      App.setTheme(appTheme.value());
    }
  }

  /**
   * Process the AppMeta annotation.
   *
   * @param clazz The class to process
   */
  private void processAppMeta(Object clazz) {
    AppMeta[] appMeta = clazz.getClass().getAnnotationsByType(AppMeta.class);
    if (appMeta != null) {
      for (AppMeta meta : appMeta) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : meta.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        getPage().setMeta(meta.name(), meta.content(), attributes);
      }
    }
  }

  /**
   * Process the StyleSheet annotation.
   *
   * @param clazz The class to process
   */
  private void processStyleSheet(Object clazz) {
    StyleSheet[] styleSheet = clazz.getClass().getAnnotationsByType(StyleSheet.class);
    if (styleSheet != null) {
      for (StyleSheet sheet : styleSheet) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : sheet.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        String key = "org.dwcj.annotations.AnnotationProcessor::styles::" + sheet.value();
        if (sheet.top()) {
          key += "::top";
        }

        if (ObjectTable.contains(key)) {
          continue;
        }

        ObjectTable.put(key, true);
        getPage().addStyleSheet(sheet.value(), sheet.top(), attributes);
      }
    }
  }

  /**
   * Process the InlineStyleSheet annotation.
   *
   * @param clazz The class to process
   */
  private void processInlineStyleSheet(Object clazz) {
    InlineStyleSheet[] inlineStyleSheets =
        clazz.getClass().getAnnotationsByType(InlineStyleSheet.class);
    if (inlineStyleSheets != null) {
      for (InlineStyleSheet sheet : inlineStyleSheets) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : sheet.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        boolean hasId = sheet.id() != null && !sheet.id().isEmpty();
        String key = "org.dwcj.annotations.AnnotationProcessor::inlineStyles::" + sheet.id();
        boolean isTracked = ObjectTable.contains(key);

        if (hasId) {
          if (isTracked) {
            continue;
          }

          attributes.put("id", sheet.id());

          if (sheet.once()) {
            ObjectTable.put(key, true);
          }
        }

        getPage().addInlineStyleSheet(sheet.value(), sheet.top(), attributes);
      }
    }
  }

  /**
   * Process the JavaScript annotation.
   *
   * @param clazz The class to process
   */
  private void processJavaScript(Object clazz) {
    JavaScript[] javaScript = clazz.getClass().getAnnotationsByType(JavaScript.class);
    if (javaScript != null) {
      for (JavaScript script : javaScript) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : script.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        String key = "org.dwcj.annotations.AnnotationProcessor::scripts::" + script.value();
        if (script.top()) {
          key += "::top";
        }

        if (ObjectTable.contains(key)) {
          continue;
        }

        ObjectTable.put(key, true);
        getPage().addJavaScript(script.value(), script.top(), attributes);
      }
    }
  }

  /**
   * Process the InlineJavaScript annotation.
   *
   * @param clazz The class to process
   */
  private void processInlineJavaScript(Object clazz) {
    InlineJavaScript[] inlineJavascript =
        clazz.getClass().getAnnotationsByType(InlineJavaScript.class);
    if (inlineJavascript != null) {
      for (InlineJavaScript script : inlineJavascript) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : script.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        boolean hasId = script.id() != null && !script.id().isEmpty();
        if (hasId) {
          String key = "org.dwcj.annotations.AnnotationProcessor::inlineScripts::" + script.id();
          boolean isTracked = ObjectTable.contains(key);

          if (isTracked) {
            continue;
          }

          attributes.put("id", script.id());
          ObjectTable.put(key, true);
        }

        getPage().addInlineJavaScript(script.value(), script.top(), attributes);
      }
    }
  }

  /**
   * Process the Link annotation.
   *
   * @param clazz The class to process
   */
  private void processLink(Object clazz) {
    Link[] links = clazz.getClass().getAnnotationsByType(Link.class);
    if (links != null) {
      for (Link link : links) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : link.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        String id = link.id() != null && !link.id().isEmpty() ? link.id() : link.value();
        String key = "org.dwcj.annotations.AnnotationProcessor::links::" + id;
        if (link.top()) {
          key += "::top";
        }

        if (ObjectTable.contains(key)) {
          continue;
        }

        ObjectTable.put(key, true);
        getPage().addLink(link.value(), link.top(), attributes);
      }
    }
  }

  /**
   * Process the Configuration annotation.
   *
   * @param clazz The class to process
   * @param phase The phase to process
   */
  private void processConfiguration(Object clazz, RunningPhase phase) {
    STConfiuration[] configurations = clazz.getClass().getAnnotationsByType(STConfiuration.class);
    if (configurations != null) {
      for (STConfiuration configuration : configurations) {
        String key = configuration.key();

        // don't allow controls to override application level configurations
        if (phase == RunningPhase.POST_RUN && StringTable.contains(key)) {
          continue;
        }

        // make sure application level configurations are not cleared
        if (phase == RunningPhase.PRE_RUN && StringTable.contains(key)) {

          // is blacklisted ?
          String[] blackList = new String[] {"DEBUG"};
          if (Arrays.asList(blackList).contains(key)) {
            continue;
          }

          StringTable.clear(key);
        }

        StringTable.put(key, configuration.value());
      }
    }
  }
}
