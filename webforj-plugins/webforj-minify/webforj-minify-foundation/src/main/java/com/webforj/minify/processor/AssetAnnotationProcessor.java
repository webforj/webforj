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
 * <p>Supports the following file-based annotations:
 *
 * <ul>
 * <li>@StyleSheet - references external CSS files that need minification
 * <li>@JavaScript - references external JS files that need minification
 * </ul>
 *
 * <p>Supported protocols:
 *
 * <ul>
 * <li>ws:// - Maps to src/main/resources/static/ (web server protocol)
 * <li>context:// - Maps to src/main/resources/ (context protocol)
 * </ul>
 *
 * <p>Unsupported (skipped with warning):
 *
 * <ul>
 * <li>http://, https:// - External CDN resources (cannot be minified)
 * <li>icons:// - Icon resources (images, not CSS/JS)
 * <li>URLs without protocol - webforJ passes these through unchanged to the browser
 * </ul>
 *
 * <p>Note: @InlineStyleSheet and @InlineJavaScript are NOT processed because they inject code
 * directly into the DOM rather than referencing external files. There are no files to minify.
 *
 * <p>The generated manifest is used by the Maven and Gradle plugins to determine which assets need
 * minification during the build process.
 *
 * @author Kevin Hagel
 */
@SupportedAnnotationTypes({
    // File-based annotations only
    "com.webforj.annotation.StyleSheet", "com.webforj.annotation.JavaScript",
    // Container annotations for repeatable file-based annotations
    "com.webforj.annotation.StyleSheet.Container",
    "com.webforj.annotation.JavaScript.Container"})
@SupportedSourceVersion(SourceVersion.RELEASE_17)
public class AssetAnnotationProcessor extends AbstractProcessor {

  private final Set<ResourceEntry> resources = new HashSet<>();
  private final Gson gson = new GsonBuilder().setPrettyPrinting().create();

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    if (roundEnv.processingOver()) {
      writeManifestIfNeeded();
      return false;
    }

    // Process each annotation type
    for (TypeElement annotation : annotations) {
      processAnnotation(annotation, roundEnv);
    }

    return false;
  }

  private void writeManifestIfNeeded() {
    if (!resources.isEmpty()) {
      writeManifest();
    }
  }

  private void processAnnotation(TypeElement annotation, RoundEnvironment roundEnv) {
    String simpleName = annotation.getSimpleName().toString();
    String qualifiedName = annotation.getQualifiedName().toString();

    if (isContainerAnnotation(simpleName)) {
      processContainerAnnotation(annotation, qualifiedName, roundEnv);
    } else {
      processIndividualAnnotation(annotation, simpleName, roundEnv);
    }
  }

  private void processContainerAnnotation(TypeElement annotation, String qualifiedName,
      RoundEnvironment roundEnv) {
    String annotationType = getAnnotationTypeFromContainer(qualifiedName);
    for (Element element : roundEnv.getElementsAnnotatedWith(annotation)) {
      if (element instanceof TypeElement) {
        extractResourceUrlsFromContainer(element, annotation, annotationType);
      }
    }
  }

  private void processIndividualAnnotation(TypeElement annotation, String simpleName,
      RoundEnvironment roundEnv) {
    String annotationType = getAnnotationType(simpleName);
    for (Element element : roundEnv.getElementsAnnotatedWith(annotation)) {
      if (element instanceof TypeElement) {
        extractResourceUrls(element, annotation, annotationType);
      }
    }
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
    if (!url.isEmpty() && isLocalResource(url)) {
      resources.add(new ResourceEntry(url, type, sourceClass));
      processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE,
          "Discovered " + type + " asset: " + url + " in " + sourceClass);
    } else if (!url.isEmpty() && !isLocalResource(url)) {
      processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE,
          "Skipping external resource (cannot be minified): " + url);
    }
  }

  private boolean isLocalResource(String url) {
    // Only process local resources that can be minified
    // External URLs (http://, https://) cannot be minified - they're hosted on CDNs
    // Icons (icons://) are images, not CSS/JS files
    if (url.startsWith("http://") || url.startsWith("https://")) {
      return false; // External CDN resources
    }
    if (url.startsWith("icons://")) {
      return false; // Icon resources (images, not minifiable)
    }

    // Check for valid protocols
    if (url.contains("://")) {
      // Has a protocol - must be ws:// or context://
      if (!url.startsWith("ws://") && !url.startsWith("context://")) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.WARNING,
            "Unknown protocol in asset URL: " + url
                + " (only ws://, context:// are supported for minification)");
        return false;
      }
    } else {
      // No protocol - webforJ passes these through unchanged to the browser
      // They cannot be reliably mapped to filesystem paths for minification
      processingEnv.getMessager().printMessage(Diagnostic.Kind.WARNING,
          "Skipping URL without protocol: " + url
              + " (use ws:// or context:// for minifiable resources)");
      return false;
    }

    // Accept: ws:// or context://
    return true;
  }

  private boolean isContainerAnnotation(String annotationName) {
    // Container annotations are inner classes (use simple name "Container")
    return annotationName.equals("Container");
  }

  private String getAnnotationTypeFromContainer(String qualifiedName) {
    // Container annotation qualified name: com.webforj.annotation.StyleSheet.Container
    // Extract "StyleSheet" from the qualified name
    if (qualifiedName.contains(".Container")) {
      String withoutContainer = qualifiedName.replace(".Container", "");
      int lastDot = withoutContainer.lastIndexOf('.');
      if (lastDot >= 0) {
        return withoutContainer.substring(lastDot + 1);
      }
    }
    return "Unknown";
  }

  private void extractResourceUrlsFromContainer(Element element,
      TypeElement containerAnnotationType, String type) {
    String sourceClass = element.toString();

    for (AnnotationMirror mirror : element.getAnnotationMirrors()) {
      if (mirror.getAnnotationType().asElement().equals(containerAnnotationType)) {
        processContainerMirror(mirror, type, sourceClass);
      }
    }
  }

  private void processContainerMirror(AnnotationMirror mirror, String type, String sourceClass) {
    Map<? extends ExecutableElement, ? extends AnnotationValue> values =
        processingEnv.getElementUtils().getElementValuesWithDefaults(mirror);

    for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> entry : values
        .entrySet()) {
      if ("value".equals(entry.getKey().getSimpleName().toString())) {
        processContainerValue(entry.getValue().getValue(), type, sourceClass);
      }
    }
  }

  private void processContainerValue(Object arrayValue, String type, String sourceClass) {
    if (!(arrayValue instanceof List)) {
      return;
    }

    @SuppressWarnings("unchecked")
    List<? extends AnnotationValue> annotationArray = (List<? extends AnnotationValue>) arrayValue;

    for (AnnotationValue annotationValue : annotationArray) {
      if (annotationValue.getValue() instanceof AnnotationMirror repeatedAnnotation) {
        processAnnotationMirror(repeatedAnnotation, type, sourceClass);
      }
    }
  }

  private String getAnnotationType(String annotationName) {
    switch (annotationName) {
      case "StyleSheet":
        return "StyleSheet";
      case "JavaScript":
        return "JavaScript";
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
    // Used by Gson, must be non-static for serialization
    @SuppressWarnings({"unused", "java:S1170"})
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
