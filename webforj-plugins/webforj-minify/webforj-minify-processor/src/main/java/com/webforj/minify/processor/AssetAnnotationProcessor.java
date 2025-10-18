package com.webforj.minify.processor;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.io.IOException;
import java.io.Writer;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import javax.tools.Diagnostic;
import javax.tools.FileObject;
import javax.tools.StandardLocation;

/**
 * Annotation processor that discovers webforJ asset annotations and generates a manifest file
 * (META-INF/webforj-resources.json) listing all discovered assets.
 *
 * <p>
 * Supports the following annotations:
 *
 * <ul>
 * <li>@StyleSheet
 * <li>@JavaScript
 * <li>@InlineStyleSheet
 * <li>@InlineJavaScript
 * </ul>
 *
 * <p>
 * The generated manifest is used by the Maven and Gradle plugins to determine which assets need
 * minification during the build process.
 */
@SupportedAnnotationTypes({"com.webforj.annotation.StyleSheet", "com.webforj.annotation.JavaScript",
    "com.webforj.annotation.InlineStyleSheet", "com.webforj.annotation.InlineJavaScript"})
@SupportedSourceVersion(SourceVersion.RELEASE_17)
public class AssetAnnotationProcessor extends AbstractProcessor {

  private final Set<ResourceEntry> resources = new HashSet<>();
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
    String sourceClass = element.toString();

    for (AnnotationMirror mirror : element.getAnnotationMirrors()) {
      if (mirror.getAnnotationType().asElement().equals(annotationType)) {
        processAnnotationMirror(mirror, type, sourceClass);
      }
    }
  }

  private void processAnnotationMirror(AnnotationMirror mirror, String type, String sourceClass) {
    Map<? extends ExecutableElement, ? extends AnnotationValue> values =
        processingEnv.getElementUtils().getElementValuesWithDefaults(mirror);

    for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry : values
        .entrySet()) {
      String paramName = entry.getKey().getSimpleName().toString();

      // Look for 'value' or 'url' parameter
      if ("value".equals(paramName) || "url".equals(paramName)) {
        processAnnotationValue(entry.getValue().getValue(), type, sourceClass);
      }
    }
  }

  private void processAnnotationValue(Object value, String type, String sourceClass) {
    if (value instanceof String url) {
      addResourceIfNotEmpty(url, type, sourceClass);
    } else if (value instanceof List) {
      processAnnotationValueList(value, type, sourceClass);
    }
  }

  private void processAnnotationValueList(Object value, String type, String sourceClass) {
    @SuppressWarnings("unchecked")
    List<? extends AnnotationValue> list = (List<? extends AnnotationValue>) value;
    for (AnnotationValue av : list) {
      String url = av.getValue().toString();
      addResourceIfNotEmpty(url, type, sourceClass);
    }
  }

  private void addResourceIfNotEmpty(String url, String type, String sourceClass) {
    if (!url.isEmpty()) {
      resources.add(new ResourceEntry(url, type, sourceClass));
      processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE,
          "Discovered " + type + " asset: " + url + " in " + sourceClass);
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
      FileObject file = processingEnv.getFiler().createResource(StandardLocation.CLASS_OUTPUT, "",
          "META-INF/webforj-resources.json");

      try (Writer writer = file.openWriter()) {
        ManifestData manifest = new ManifestData(new ArrayList<>(resources));
        gson.toJson(manifest, writer);
        processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE,
            "Generated META-INF/webforj-resources.json with " + resources.size()
                + " unique resource(s)");
      }
    } catch (IOException e) {
      processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR,
          "Failed to write manifest: " + e.getMessage());
    }
  }

  /**
   * Represents a single resource entry in the manifest.
   */
  private static class ResourceEntry {
    @SuppressWarnings("unused") // Used by Gson
    private final String url;

    @SuppressWarnings("unused") // Used by Gson
    private final String type;

    @SuppressWarnings("unused") // Used by Gson
    private final String discoveredIn;

    public ResourceEntry(String url, String type, String discoveredIn) {
      this.url = url;
      this.type = type;
      this.discoveredIn = discoveredIn;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (!(o instanceof ResourceEntry)) {
        return false;
      }
      ResourceEntry that = (ResourceEntry) o;
      return url.equals(that.url) && type.equals(that.type);
    }

    @Override
    public int hashCode() {
      return url.hashCode() * 31 + type.hashCode();
    }
  }

  /**
   * Root object for the manifest JSON structure.
   */
  private static class ManifestData {
    @SuppressWarnings({"unused", "java:S1170"}) // Used by Gson, must be non-static for
                                                // serialization
    private final String version = "1.0";

    @SuppressWarnings("unused") // Used by Gson
    private final String generatedAt = Instant.now().toString();

    @SuppressWarnings("unused") // Used by Gson
    private final List<ResourceEntry> assets;

    public ManifestData(List<ResourceEntry> resources) {
      this.assets = resources;
    }
  }
}
