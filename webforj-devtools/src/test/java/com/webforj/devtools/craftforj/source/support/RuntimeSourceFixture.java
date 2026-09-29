package com.webforj.devtools.craftforj.source.support;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.source.site.ComponentSiteResolver;
import com.webforj.devtools.craftforj.source.site.ComponentSiteResolvers;
import com.webforj.devtools.craftforj.utilities.ComponentTree;
import com.webforj.environment.ObjectTable;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.Constructor;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import javax.tools.ToolProvider;
import org.mockito.MockedStatic;

/**
 * Compiles application source, runs it, and hands out the components it really created.
 */
public final class RuntimeSourceFixture implements AutoCloseable {

  private final Path sources;
  private final Path classes;
  private final Map<String, Path> files = new LinkedHashMap<>();
  private final Map<String, Object> table = new HashMap<>();
  private final List<Component> roots = new ArrayList<>();
  private final ClassLoader previous = Thread.currentThread().getContextClassLoader();
  private final MockedStatic<Environment> environment = mockStatic(Environment.class);
  private final MockedStatic<ObjectTable> objectTable = mockStatic(ObjectTable.class);
  private final MockedStatic<SourceFileResolver> resolver = mockStatic(SourceFileResolver.class);
  private final MockedStatic<SourcePathRegistry> paths = mockStatic(SourcePathRegistry.class);
  private ClassLoader loader;

  /**
   * Creates a fixture that keeps its files below the given directory.
   *
   * @param root the directory that receives the source and the class files
   */
  public RuntimeSourceFixture(Path root) {
    this.sources = root.resolve("src");
    this.classes = root.resolve("classes");

    Environment debug = mock(Environment.class);
    when(debug.isDebug()).thenReturn(true);
    environment.when(Environment::getCurrent).thenReturn(debug);
    objectTable.when(() -> ObjectTable.contains(anyString()))
        .thenAnswer(call -> table.containsKey(call.getArgument(0)));
    objectTable.when(() -> ObjectTable.get(anyString()))
        .thenAnswer(call -> table.get(call.getArgument(0)));
    objectTable.when(() -> ObjectTable.put(anyString(), any()))
        .thenAnswer(call -> table.put(call.getArgument(0), call.getArgument(1)));
    resolver.when(() -> SourceFileResolver.resolve(anyString(), any()))
        .thenAnswer(call -> findFile(call.getArgument(0)));
    resolver.when(() -> SourceFileResolver.resolve(anyString(), any(), any()))
        .thenAnswer(call -> findFile(call.getArgument(0)));
    paths.when(() -> SourcePathRegistry.isRecorded(anyString()))
        .thenAnswer(call -> files.containsValue(Path.of((String) call.getArgument(0))));
  }

  /**
   * Writes the source file of a class.
   *
   * @param className the fully qualified name of the top level class of the file
   * @param source the complete file content
   * @return the written file
   */
  public Path addSource(String className, String source) {
    Path file = sources.resolve(className.replace('.', '/') + ".java");
    write(file, source);
    files.put(className, file);

    return file;
  }

  /**
   * Compiles every source file and makes the classes the ones the application loads.
   */
  public void compile() {
    try {
      Files.createDirectories(classes);
      List<String> arguments = new ArrayList<>(List.of("-proc:none", "-classpath",
          System.getProperty("java.class.path"), "-d", classes.toString()));
      files.values().forEach(file -> arguments.add(file.toString()));
      ByteArrayOutputStream errors = new ByteArrayOutputStream();
      int result = ToolProvider.getSystemJavaCompiler().run(null, errors, errors,
          arguments.toArray(String[]::new));
      assertEquals(0, result, errors.toString());

      if (loader == null) {
        loader = new CompiledClassLoader(classes, previous);
        Thread.currentThread().setContextClassLoader(loader);
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * Creates an object of a compiled class.
   *
   * @param <T> the type the caller reads the object as
   * @param className the fully qualified class name
   * @param arguments the constructor arguments
   * @return the object
   */
  @SuppressWarnings("unchecked")
  public <T> T create(String className, Object... arguments) {
    try {
      Class<?> type = Class.forName(className, true, loader);
      for (Constructor<?> constructor : type.getDeclaredConstructors()) {
        if (constructor.getParameterCount() == arguments.length) {
          constructor.setAccessible(true);
          T created = (T) constructor.newInstance(arguments);
          if (created instanceof Component component) {
            roots.add(component);
          }

          return created;
        }
      }

      throw new IllegalArgumentException(className + " has no such constructor");
    } catch (ReflectiveOperationException e) {
      throw new IllegalStateException(e);
    }
  }

  /**
   * Makes a component and everything below it a part of the running application.
   *
   * @param <T> the component type
   * @param root the component
   * @return the component
   */
  public <T extends Component> T show(T root) {
    roots.add(root);

    return root;
  }

  /**
   * Gets the components of the running application.
   *
   * @return every shown component and what it holds
   */
  public List<Component> getComponents() {
    List<Component> components = new ArrayList<>();
    for (Component root : roots) {
      if (components.stream().noneMatch(known -> known == root)) {
        components.add(root);
      }

      ComponentTree.findBelow(root).stream()
          .filter(below -> components.stream().noneMatch(known -> known == below))
          .forEach(components::add);
    }

    return components;
  }

  /**
   * Finds one component of the running application.
   *
   * @param <T> the component type
   * @param type the component class
   * @param filter tells the wanted component apart
   * @return the only component of that class the filter accepts
   */
  public <T extends Component> T find(Class<T> type, Predicate<T> filter) {
    List<T> matches =
        getComponents().stream().filter(type::isInstance).map(type::cast).filter(filter).toList();
    assertEquals(1, matches.size(), "one " + type.getSimpleName() + " was expected to match");

    return matches.get(0);
  }

  /**
   * Creates the resolver of the expressions the components of the application stand for.
   *
   * @return the resolver
   */
  public ComponentSiteResolver createSiteResolver() {
    return ComponentSiteResolvers.create(this::getComponents, ComponentTree::findBelow);
  }

  /** {@inheritDoc} */
  @Override
  public void close() {
    Thread.currentThread().setContextClassLoader(previous);
    paths.close();
    resolver.close();
    objectTable.close();
    environment.close();
  }

  private String findFile(String className) {
    int nested = className.indexOf('$');
    Path file = files.get(nested < 0 ? className : className.substring(0, nested));

    return file == null ? null : file.toString();
  }

  private static void write(Path file, String content) {
    try {
      Files.createDirectories(file.getParent());
      Files.writeString(file, content);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * Loads the compiled classes as they are on disk, a class that names no location is left alone by
   * the tools that rewrite classes while they load.
   */
  private static final class CompiledClassLoader extends ClassLoader {

    private final Path classes;

    CompiledClassLoader(Path classes, ClassLoader parent) {
      super(parent);
      this.classes = classes;
    }

    /** {@inheritDoc} */
    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
      Path file = classes.resolve(name.replace('.', '/') + ".class");
      if (!Files.isRegularFile(file)) {
        throw new ClassNotFoundException(name);
      }

      try {
        byte[] content = Files.readAllBytes(file);

        return defineClass(name, content, 0, content.length);
      } catch (IOException e) {
        throw new ClassNotFoundException(name, e);
      }
    }

    /** {@inheritDoc} */
    @Override
    protected URL findResource(String name) {
      Path file = classes.resolve(name);
      try {
        return Files.isRegularFile(file) ? file.toUri().toURL() : null;
      } catch (MalformedURLException e) {
        throw new UncheckedIOException(e);
      }
    }
  }
}
