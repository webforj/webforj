package com.webforj.minify.processor;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Annotation processor that discovers webforJ asset annotations and generates
 * a manifest file (META-INF/webforj-resources.json) listing all discovered assets.
 *
 * Supports the following annotations:
 * <ul>
 *   <li>@StyleSheet</li>
 *   <li>@JavaScript</li>
 *   <li>@InlineStyleSheet</li>
 *   <li>@InlineJavaScript</li>
 * </ul>
 */
@SupportedAnnotationTypes({
  "com.webforj.annotation.StyleSheet",
  "com.webforj.annotation.JavaScript",
  "com.webforj.annotation.InlineStyleSheet",
  "com.webforj.annotation.InlineJavaScript"
})
@SupportedSourceVersion(SourceVersion.RELEASE_11)
public class AssetAnnotationProcessor extends AbstractProcessor {

  private final List<ResourceEntry> resources = new ArrayList<>();
  private final Gson gson = new GsonBuilder().setPrettyPrinting().create();

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    if (roundEnv.processingOver()) {
      // Final round - write the manifest
      if (!resources.isEmpty()) {
        writeManifest();
      }
      return false;
    }

    // Process each annotation type
    for (TypeElement annotation : annotations) {
      String annotationType = getAnnotationType(annotation.getSimpleName().toString());

      for (Element element : roundEnv.getElementsAnnotatedWith(annotation)) {
        // Only process class-level annotations
        if (element instanceof TypeElement) {
          extractResourceUrls(element, annotation, annotationType);
        }
      }
    }

    return false;
  }

  private void extractResourceUrls(Element element, TypeElement annotationType, String type) {
    for (AnnotationMirror mirror : element.getAnnotationMirrors()) {
      if (mirror.getAnnotationType().asElement().equals(annotationType)) {
        Map<? extends ExecutableElement, ? extends AnnotationValue> values =
          processingEnv.getElementUtils().getElementValuesWithDefaults(mirror);

        for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry : values.entrySet()) {
          String paramName = entry.getKey().getSimpleName().toString();

          // Look for 'value' or 'url' parameter
          if ("value".equals(paramName) || "url".equals(paramName)) {
            Object value = entry.getValue().getValue();

            if (value instanceof String) {
              String url = (String) value;
              if (!url.isEmpty()) {
                resources.add(new ResourceEntry(url, type));
              }
            } else if (value instanceof List) {
              // Handle array of values
              @SuppressWarnings("unchecked")
              List<? extends AnnotationValue> list = (List<? extends AnnotationValue>) value;
              for (AnnotationValue av : list) {
                String url = av.getValue().toString();
                if (!url.isEmpty()) {
                  resources.add(new ResourceEntry(url, type));
                }
              }
            }
          }
        }
      }
    }
  }

  private String getAnnotationType(String annotationName) {
    switch (annotationName) {
      case "StyleSheet":
        return "StyleSheet";
      case "JavaScript":
        return "JavaScript";
      case "InlineStyleSheet":
        return "InlineStyleSheet";
      case "InlineJavaScript":
        return "InlineJavaScript";
      default:
        return "Unknown";
    }
  }

  private void writeManifest() {
    try {
      FileObject file = processingEnv.getFiler().createResource(
        StandardLocation.CLASS_OUTPUT,
        "",
        "META-INF/webforj-resources.json"
      );

      try (Writer writer = file.openWriter()) {
        ManifestData manifest = new ManifestData(resources);
        gson.toJson(manifest, writer);
        processingEnv.getMessager().printMessage(
          javax.tools.Diagnostic.Kind.NOTE,
          "Generated webforj-resources.json with " + resources.size() + " resource(s)"
        );
      }
    } catch (IOException e) {
      processingEnv.getMessager().printMessage(
        javax.tools.Diagnostic.Kind.ERROR,
        "Failed to write manifest: " + e.getMessage()
      );
    }
  }

  /**
   * Represents a single resource entry in the manifest.
   */
  private static class ResourceEntry {
    private final String url;
    private final String type;

    public ResourceEntry(String url, String type) {
      this.url = url;
      this.type = type;
    }
  }

  /**
   * Root object for the manifest JSON structure.
   */
  private static class ManifestData {
    private final List<ResourceEntry> resources;

    public ManifestData(List<ResourceEntry> resources) {
      this.resources = resources;
    }
  }
}
