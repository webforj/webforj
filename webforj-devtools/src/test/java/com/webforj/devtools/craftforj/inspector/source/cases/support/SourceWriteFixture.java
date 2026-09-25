package com.webforj.devtools.craftforj.inspector.source.cases.support;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import com.google.gson.Gson;
import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.mockito.MockedStatic;

/** Supplies temporary source files, component identity and runtime locations to writer cases. */
public final class SourceWriteFixture implements AutoCloseable {

  private final Path root;
  private final Map<String, Path> sources = new LinkedHashMap<>();
  private final Map<String, Component> components = new LinkedHashMap<>();
  private final FeatureHandlerRegistry registry = new FeatureHandlerRegistry();
  private final Gson gson = new Gson();
  private final SourceCodeModifier modifier =
      new SourceCodeModifier(registry, new SourceParserService());
  private final MockedStatic<ComponentLocator> locator = mockStatic(ComponentLocator.class);
  private final MockedStatic<ComponentSourceRegistry> sourceRegistry =
      mockStatic(ComponentSourceRegistry.class);
  private final MockedStatic<SourceFileResolver> resolver = mockStatic(SourceFileResolver.class);
  private final MockedStatic<SourcePathRegistry> paths = mockStatic(SourcePathRegistry.class);

  /**
   * Creates a fixture rooted at the given directory.
   *
   * @param root the directory that receives the application source files
   */
  public SourceWriteFixture(Path root) {
    this.root = root;
    locator.when(() -> ComponentLocator.findById(anyString()))
        .thenAnswer(call -> Optional.ofNullable(components.get(call.getArgument(0))));
    resolver.when(() -> SourceFileResolver.resolve(anyString(), any())).thenAnswer(call -> {
      Path file = sources.get(call.getArgument(0));
      return file == null ? null : file.toString();
    });
    paths.when(() -> SourcePathRegistry.isRecorded(anyString()))
        .thenAnswer(call -> sources.containsValue(Path.of((String) call.getArgument(0))));
  }

  /**
   * Writes an application source file and records its location for the class.
   *
   * @param className the fully qualified class name
   * @param source the complete file content
   * @return the written file
   * @throws IOException when the file cannot be written
   */
  public Path addSource(String className, String source) throws IOException {
    Path file = root.resolve(className.replace('.', '/') + ".java");
    Files.createDirectories(file.getParent());
    Files.writeString(file, source);
    sources.put(className, file);
    return file;
  }

  /**
   * Records another class declared in an existing source file.
   *
   * @param className the fully qualified class name
   * @param file the file that declares it
   */
  public void addSourceClass(String className, Path file) {
    sources.put(className, file);
  }

  /** Resolves source files through the production resolver instead of the recorded map. */
  public void useRealSourceResolution() {
    resolver.when(() -> SourceFileResolver.resolve(anyString(), any())).thenCallRealMethod();
    resolver.when(() -> SourceFileResolver.resolve(anyString(), any(), any())).thenCallRealMethod();
  }

  /**
   * Registers a live component with the source chain the runtime captured for it.
   *
   * @param <T> the component type
   * @param id the component id
   * @param type the component class
   * @param chain the runtime source chain, innermost first
   * @return the registered component
   */
  public <T extends Component> T addComponent(String id, Class<T> type, List<SourcePoint> chain) {
    T component = mock(type);
    components.put(id, component);
    sourceRegistry.when(() -> ComponentSourceRegistry.getSourcePoint(component))
        .thenReturn(chain.isEmpty() ? null : chain.get(0));
    sourceRegistry.when(() -> ComponentSourceRegistry.getSourceChain(component)).thenReturn(chain);
    return component;
  }

  /**
   * Removes a component's runtime identity while keeping its source files.
   *
   * @param id the component id
   */
  public void removeComponent(String id) {
    components.remove(id);
  }

  /**
   * Builds a client edit through the real handler for the feature.
   *
   * @param id the component id
   * @param feature the feature type
   * @param value the requested value, empty for a reset
   * @return the change request as the client would send it
   */
  public ChangeRequest createChange(String id, String feature, Object value) {
    FeatureProperty property =
        registry.getHandler(feature).orElseThrow().get(components.get(id)).orElseThrow();
    FeatureProperty edited = FeatureProperty.builder(property.getName(), property.getFeatureType())
        .javaType(property.getJavaType()).value(value).build();
    FeatureProperty requestProperty = gson.fromJson(gson.toJson(edited), FeatureProperty.class);
    return new ChangeRequest(id, requestProperty, null);
  }

  /**
   * Gets the writer with every production handler loaded.
   *
   * @return the writer
   */
  public SourceCodeModifier getModifier() {
    return modifier;
  }

  /** {@inheritDoc} */
  @Override
  public void close() {
    paths.close();
    resolver.close();
    sourceRegistry.close();
    locator.close();
  }
}
