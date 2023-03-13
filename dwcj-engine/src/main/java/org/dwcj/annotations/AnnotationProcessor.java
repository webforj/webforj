package org.dwcj.annotations;

import java.util.HashMap;
import org.dwcj.App;
import org.dwcj.controls.AbstractControl;
import org.dwcj.environment.ObjectTable;
import org.dwcj.exceptions.DwcAnnotationException;
import org.dwcj.exceptions.DwcException;

/**
 * Annotation processor for the application and controls annotations
 * 
 * @author Hyyan Abo Fakher
 */
public final class AnnotationProcessor {

  /**
   * The running phase of the application
   */
  public enum RunningPhase {
    /**
     * The application is running before the run method
     */
    PRE_RUN,
    /**
     * The application is running after the run method
     */
    POST_RUN
  }

  /**
   * Process the annotations of the application
   * 
   * @param app   The application to process
   * @param phase The phase of the application
   * @throws DwcAnnotationException If the annotation processing failed
   */
  public void processAppAnnotations(App app, RunningPhase phase) throws DwcAnnotationException {
    try {
      if (phase == RunningPhase.PRE_RUN) {
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

    } catch (DwcException e) {
      throw new DwcAnnotationException("Failed to process app annotations", e);
    }
  }

  /**
   * Process the annotations of the control
   * 
   * @param control The control to process
   * @throws DwcAnnotationException If the annotation processing failed
   */
  public void processControlAnnotations(AbstractControl control) throws DwcAnnotationException {
    try {
      processLink(control);
      processStyleSheet(control);
      processInlineStyleSheet(control);
      processJavaScript(control);
      processInlineJavaScript(control);
    } catch (DwcException e) {
      throw new DwcAnnotationException("Failed to process control annotations", e);
    }
  }

  /**
   * Process the AppTitle annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processAppTitle(Object clazz) throws DwcException {
    AppTitle appTitle = clazz.getClass().getAnnotation(AppTitle.class);
    if (appTitle != null) {
      App.getPage().setTitle(appTitle.value());
    }
  }

  /**
   * Process the AppAttribute annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processAppAttribute(Object clazz) throws DwcException {
    AppAttribute[] appAttributes = clazz.getClass().getAnnotationsByType(AppAttribute.class);
    if (appAttributes != null) {
      for (AppAttribute appAttribute : appAttributes) {
        App.getPage().setAttribute(appAttribute.name(), appAttribute.value(), appAttribute.selector());
      }
    }
  }

  /**
   * Process the AppDarkTheme annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processAppDarkTheme(Object clazz) throws DwcException {
    AppDarkTheme appDarkTheme = clazz.getClass().getAnnotation(AppDarkTheme.class);
    if (appDarkTheme != null) {
      App.setDarkTheme(appDarkTheme.value());
    }
  }

  /**
   * Process the AppLightTheme annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processAppLightTheme(Object clazz) throws DwcException {
    AppLightTheme appLightTheme = clazz.getClass().getAnnotation(AppLightTheme.class);
    if (appLightTheme != null) {
      App.setLightTheme(appLightTheme.value());
    }
  }

  /**
   * Process the AppTheme annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processAppTheme(Object clazz) throws DwcException {
    AppTheme appTheme = clazz.getClass().getAnnotation(AppTheme.class);
    if (appTheme != null) {
      App.setTheme(appTheme.value());
    }
  }

  /**
   * Process the AppMeta annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processAppMeta(Object clazz) throws DwcException {
    AppMeta[] appMeta = clazz.getClass().getAnnotationsByType(AppMeta.class);
    if (appMeta != null) {
      for (AppMeta meta : appMeta) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : meta.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        App.getPage().setMeta(meta.name(), meta.content(), attributes);
      }
    }
  }

  /**
   * Process the StyleSheet annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processStyleSheet(Object clazz) throws DwcException {
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
        App.getPage().addStyleSheet(sheet.value(), sheet.top(), attributes);
      }
    }
  }

  /**
   * Process the InlineStyleSheet annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processInlineStyleSheet(Object clazz) throws DwcException {
    InlineStyleSheet[] inlineStyleSheets = clazz.getClass().getAnnotationsByType(InlineStyleSheet.class);
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

        App.getPage().addInlineStyleSheet(sheet.value(), sheet.top(), attributes);
      }
    }
  }

  /**
   * Process the JavaScript annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processJavaScript(Object clazz) throws DwcException {
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
        App.getPage().addJavaScript(script.value(), script.top(), attributes);
      }
    }
  }

  /**
   * Process the InlineJavaScript annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processInlineJavaScript(Object clazz) throws DwcException {
    InlineJavaScript[] inlineJavascript = clazz.getClass().getAnnotationsByType(InlineJavaScript.class);
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

        App.getPage().addInlineJavaScript(script.value(), script.top(), attributes);
      }
    }
  }

  /**
   * Process the Link annotation
   * 
   * @param clazz The class to process
   * @throws DwcException If the annotation processing failed
   */
  private void processLink(Object clazz) throws DwcException {
    Link[] links = clazz.getClass().getAnnotationsByType(Link.class);
    if (links != null) {
      for (Link link : links) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : link.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        if (link.id() != null && !link.id().isEmpty()) {
          attributes.put("id", link.id());
        }

        App.getPage().addLink(link.value(), link.top(), attributes);
      }
    }
  }
}
