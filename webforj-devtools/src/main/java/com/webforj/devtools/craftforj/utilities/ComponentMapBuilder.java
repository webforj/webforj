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
      Component bound = ComponentUtil.getBoundComponent((Composite<?>) component);
      if (bound != null) {
        compositeComponentType = bound.getClass().getName();
      }
    }

    SourceLocation source = createSourceLocation(component);
    SourceLocation usageSource = createUsageSourceLocation(component);

    return new ComponentMeta(component.getComponentId(), component.getClass().getName(),
        compositeComponentType, component.getClass().getSimpleName(), isComposite, source,
        usageSource);
  }

  private SourceLocation createSourceLocation(Component component) {
    SourcePoint sourcePoint = ComponentSourceRegistry.getSourcePoint(component);
    if (sourcePoint == null) {
      return null;
    }

    String file =
        SourceFileResolver.resolve(sourcePoint.className(), SourceFileResolver.ALL_EXTENSIONS);
    if (file == null) {
      return null;
    }

    SourcePathRegistry.record(file);

    int line = sourcePoint.lineNumber();
    String declaringClass = sourcePoint.className();
    String componentType = component.getClass().getName();
    String variableName = parserService.extractVariableName(Path.of(file), line,
        ComponentTypeNames.of(component.getClass()));

    return new SourceLocation(file, line, declaringClass, variableName, componentType);
  }

  /**
   * Finds the nearest caller frame beyond the creation frame that resolves to a project source file
   * different from the creation file, i.e. where the component's enclosing class was used.
   *
   * @param component the component
   * @return the usage source location, or null if not available
   */
  private SourceLocation createUsageSourceLocation(Component component) {
    List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);
    if (chain.size() < 2) {
      return null;
    }

    String creationFile =
        SourceFileResolver.resolve(chain.get(0).className(), SourceFileResolver.JAVA_ONLY);

    for (int i = 1; i < chain.size(); i++) {
      SourcePoint point = chain.get(i);
      String file = SourceFileResolver.resolve(point.className(), SourceFileResolver.JAVA_ONLY);
      if (file == null) {
        continue;
      }

      if (isSameFile(file, creationFile)) {
        continue;
      }

      SourcePathRegistry.record(file);

      return new SourceLocation(file, point.lineNumber(), point.className(), null, null);
    }

    return null;
  }

  private boolean isSameFile(String file, String otherFile) {
    return otherFile != null && otherFile.equals(file);
  }
}
