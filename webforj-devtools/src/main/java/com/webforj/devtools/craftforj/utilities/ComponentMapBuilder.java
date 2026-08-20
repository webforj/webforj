package com.webforj.devtools.craftforj.utilities;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.DwcComponent;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasComponents;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.model.ComponentMeta;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Builds a map of component metadata from the current application state.
 *
 * <p>
 * This class traverses all components registered with the application and collects their metadata
 * into a flat map keyed by clientId (dwc-id).
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ComponentMapBuilder {

  private final SourceParserService parserService;

  /** Creates a new builder with the given parser service. */
  public ComponentMapBuilder(SourceParserService parserService) {
    this.parserService = parserService;
  }

  /**
   * Builds a flat map of component metadata keyed by clientId.
   *
   * @return map of clientId to list of ComponentMeta (the compositeStack)
   */
  public Map<String, List<ComponentMeta>> buildComponentMap() {
    Map<String, List<ComponentMeta>> componentMap = new HashMap<>();
    Set<String> visited = new HashSet<>();

    for (Frame frame : App.getFrames()) {
      collectComponents(frame, componentMap, visited, new ArrayList<>());
    }

    return componentMap;
  }

  private void collectComponents(Component component, Map<String, List<ComponentMeta>> map,
      Set<String> visited, List<ComponentMeta> compositeStack) {
    String componentId = component.getComponentId();

    // Skip if null or already visited
    if (componentId == null || visited.contains(componentId)) {
      return;
    }

    visited.add(componentId);

    if (component instanceof Frame frame) {
      processFrame(frame, map, visited);
      return;
    }

    if (component instanceof Composite<?> composite) {
      processComposite(composite, map, visited, compositeStack);
      return;
    }

    if (component instanceof Element element) {
      processElement(element, map, visited, compositeStack);
      return;
    }

    if (component instanceof DwcComponent<?> dwc) {
      processDwcComponent(dwc, component, map, visited, compositeStack);
    }
  }

  private void processFrame(Frame frame, Map<String, List<ComponentMeta>> map,
      Set<String> visited) {
    String clientId = frame.getClientComponentId();
    if (clientId != null && !clientId.isEmpty()) {
      List<ComponentMeta> stack = new ArrayList<>();
      stack.add(createComponentMeta(frame));
      map.put(clientId, stack);
    }

    for (Component child : frame.getComponents()) {
      collectComponents(child, map, visited, new ArrayList<>());
    }
  }

  private void processComposite(Composite<?> composite, Map<String, List<ComponentMeta>> map,
      Set<String> visited, List<ComponentMeta> compositeStack) {
    List<ComponentMeta> newStack = new ArrayList<>(compositeStack);
    newStack.add(createComponentMeta(composite));

    Component bound = ComponentUtil.getBoundComponent(composite);
    if (bound != null) {
      collectComponents(bound, map, visited, newStack);
    }
  }

  private void processElement(Element element, Map<String, List<ComponentMeta>> map,
      Set<String> visited, List<ComponentMeta> compositeStack) {
    String clientId = element.getClientComponentId();
    if (clientId != null && !clientId.isEmpty()) {
      List<ComponentMeta> stack;

      if (compositeStack.isEmpty()) {
        stack = new ArrayList<>();
        stack.add(createComponentMeta(element));
      } else {
        stack = new ArrayList<>(compositeStack);
      }
      map.put(clientId, stack);

      for (Component child : element.getComponents()) {
        collectComponents(child, map, visited, new ArrayList<>());
      }
    }
  }

  private void processDwcComponent(DwcComponent<?> dwc, Component component,
      Map<String, List<ComponentMeta>> map, Set<String> visited,
      List<ComponentMeta> compositeStack) {
    String clientId = dwc.getClientComponentId();
    if (clientId != null && !clientId.isEmpty()) {
      List<ComponentMeta> stack;

      if (compositeStack.isEmpty()) {
        stack = new ArrayList<>();
        stack.add(createComponentMeta(component));
      } else {
        stack = new ArrayList<>(compositeStack);
      }
      map.put(clientId, stack);

      if (component instanceof HasComponents container) {
        for (Component child : container.getComponents()) {
          collectComponents(child, map, visited, new ArrayList<>());
        }
      }
    }
  }

  private ComponentMeta createComponentMeta(Component component) {
    boolean isComposite =
        component instanceof Composite<?> && !(component instanceof ElementComposite);

    String compositeComponentType = component.getClass().getName();
    if (isComposite) {
      Component bound = ComponentUtil.getBoundComponent(component);
      if (bound != null) {
        compositeComponentType = bound.getClass().getName();
      }
    }

    List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);
    int declaringIndex = findDeclaringIndex(chain);
    SourceLocation source = createSourceLocation(component, chain, declaringIndex);
    SourceLocation usageSource = createUsageSourceLocation(chain, declaringIndex);

    ComponentMeta meta = new ComponentMeta(component.getComponentId(),
        component.getClass().getName(), compositeComponentType,
        component.getClass().getSimpleName(), isComposite, source, usageSource);
    meta.setKotlin(isKotlinDeclared(component, chain, declaringIndex));

    return meta;
  }

  /**
   * Finds the first recorded frame that resolves to a project source file. Frame 0 is the creation
   * site, but a Kotlin DSL call puts a library function there, so the declaring file sits deeper in
   * the chain.
   *
   * @param chain the recorded source chain, creation frame first
   * @return the index of the declaring frame, or -1 when no frame resolves
   */
  private int findDeclaringIndex(List<SourcePoint> chain) {
    for (int i = 0; i < chain.size(); i++) {
      if (resolveDeclaringFile(chain.get(i)) != null) {
        return i;
      }
    }

    return -1;
  }

  private String resolveDeclaringFile(SourcePoint point) {
    return SourceFileResolver.resolve(getOuterClassName(point.className()),
        SourceFileResolver.ALL_EXTENSIONS);
  }

  private boolean isKotlinDeclared(Component component, List<SourcePoint> chain,
      int declaringIndex) {
    if (declaringIndex >= 0) {
      return KotlinClassDetector.isKotlin(chain.get(declaringIndex).className(),
          component.getClass().getClassLoader());
    }

    return KotlinClassDetector.isKotlin(component.getClass());
  }

  private SourceLocation createSourceLocation(Component component, List<SourcePoint> chain,
      int declaringIndex) {
    if (declaringIndex < 0) {
      return null;
    }

    SourcePoint point = chain.get(declaringIndex);
    String file = resolveDeclaringFile(point);
    SourcePathRegistry.addPath(file);

    int line = point.lineNumber();
    String declaringClass = getOuterClassName(point.className());
    String componentType = component.getClass().getName();
    String variableName = parserService.extractVariableName(Path.of(file), line,
        ComponentTypeNames.of(component.getClass()));

    return new SourceLocation(file, line, declaringClass, variableName, componentType);
  }

  /**
   * Finds the nearest caller frame beyond the declaring frame that resolves to a different project
   * source file, which is where the component's enclosing class was used.
   *
   * @param chain the recorded source chain, creation frame first
   * @param declaringIndex the index of the declaring frame, or -1 when none resolved
   * @return the usage source location, or null if not available
   */
  private SourceLocation createUsageSourceLocation(List<SourcePoint> chain, int declaringIndex) {
    int start = Math.max(declaringIndex, 0);
    if (chain.size() < start + 2) {
      return null;
    }

    String creationFile = SourceFileResolver
        .resolve(getOuterClassName(chain.get(start).className()), SourceFileResolver.JAVA_ONLY);

    for (int i = start + 1; i < chain.size(); i++) {
      SourcePoint point = chain.get(i);
      String file = SourceFileResolver.resolve(getOuterClassName(point.className()),
          SourceFileResolver.JAVA_ONLY);
      if (file == null) {
        continue;
      }

      if (isSameFile(file, creationFile)) {
        continue;
      }

      SourcePathRegistry.addPath(file);

      return new SourceLocation(file, point.lineNumber(), point.className(), null, null);
    }

    return null;
  }

  private static String getOuterClassName(String className) {
    int nested = className.indexOf('$');
    return nested > 0 ? className.substring(0, nested) : className;
  }

  private boolean isSameFile(String file, String otherFile) {
    return otherFile != null && otherFile.equals(file);
  }
}
