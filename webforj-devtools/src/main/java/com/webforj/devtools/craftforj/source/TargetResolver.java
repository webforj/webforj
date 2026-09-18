package com.webforj.devtools.craftforj.source;

import com.github.javaparser.Range;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.element.ElementComposite;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import com.webforj.devtools.craftforj.utilities.ComponentTypeNames;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Resolves where in the source tree a change must be written.
 *
 * <p>
 * Live components resolve through the component source registry. Destroyed components fall back to
 * the location the client stored and are re-anchored against the current AST through an ordered
 * list of {@link ReanchorRule} instances, so new re-anchoring cases plug in as standalone rules.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class TargetResolver {

  private final SourceParserService parserService;
  private final List<ReanchorRule> reanchorRules;

  /**
   * A single re-anchoring attempt for a destroyed component's stored location.
   */
  @FunctionalInterface
  interface ReanchorRule {

    /**
     * Attempts to re-anchor the given location against the parsed file.
     *
     * @param cu the parsed file the change targets
     * @param location the stored location to re-anchor
     * @return the re-anchored location, or {@code null} to let the next rule try
     */
    SourceLocation reanchor(CompilationUnit cu, SourceLocation location);
  }

  /**
   * Creates a resolver.
   *
   * @param parserService the parser service
   */
  public TargetResolver(SourceParserService parserService) {
    this.parserService = parserService;
    this.reanchorRules = List.of(this::redirectCompositeAlias, this::reanchorByDeclaration);
  }

  /**
   * Resolves the source location for a component, preferring runtime information.
   *
   * @param component the live component, or {@code null} when it was destroyed
   * @param fallback the client-stored location used when the component is gone
   *
   * @return the resolved location, or {@code null} when none can be trusted
   */
  public SourceLocation resolve(Component component, SourceLocation fallback) {
    if (component != null) {
      // Component exists in runtime - build fresh SourceLocation
      SourcePoint sourcePoint = ComponentSourceRegistry.getSourcePoint(component);
      if (sourcePoint == null) {
        return null;
      }

      String file = resolveSourceFile(sourcePoint.className());
      if (file == null) {
        return null;
      }

      int line = sourcePoint.lineNumber();
      String declaringClass = sourcePoint.className();
      String componentType = component.getClass().getName();
      String variableName = parserService.extractVariableName(Path.of(file), line,
          ComponentTypeNames.of(component.getClass()));

      return new SourceLocation(file, line, declaringClass, variableName, componentType);
    }

    // Component destroyed - honor the client fallback only when it points at a source file the
    // server itself resolved earlier
    if (fallback != null && fallback.hasBasicInfo()
        && SourcePathRegistry.isRecorded(fallback.getFile())) {
      return fallback;
    }

    return null;
  }

  /**
   * Resolves the source location of a parent layout.
   *
   * @param parentId the id of the parent component, or {@code null} when unknown
   * @param parentSource the client-stored parent location used when the parent is gone
   *
   * @return the parent location, or {@code null} when none can be trusted
   */
  public SourceLocation resolveParent(String parentId, SourceLocation parentSource) {
    return resolve(resolveParentComponent(parentId), parentSource);
  }

  /**
   * Resolves the parent component a change is scoped to.
   *
   * @param parentId the id of the parent component, or {@code null} when unknown
   * @return the parent component, or {@code null} when it is gone
   */
  public Component resolveParentComponent(String parentId) {
    Component parent = parentId != null ? ComponentLocator.findById(parentId).orElse(null) : null;

    // A Composite wrapping the layout is created in the outer file, but the item calls belong in
    // the composite's own file where the bound layout and its children live
    return unwrapBoundComponent(parent);
  }

  /**
   * Unwraps a composite to the component it binds, when one exists.
   *
   * @param component the component to unwrap
   * @return the bound component, or the input when nothing is bound
   */
  public Component unwrapBoundComponent(Component component) {
    if (component instanceof Composite<?> && !(component instanceof ElementComposite)) {
      Component bound = ComponentUtil.getBoundComponent(component);
      if (bound != null) {
        return bound;
      }
    }

    return component;
  }

  /**
   * Re-derives a destroyed component's stored location from the current AST.
   *
   * @param cu the parsed file the change targets
   * @param location the stored location to re-anchor
   * @return the re-anchored location, or the input when no rule improved it
   */
  public SourceLocation reanchorDestroyedLocation(CompilationUnit cu, SourceLocation location) {
    for (ReanchorRule rule : reanchorRules) {
      SourceLocation reanchored = rule.reanchor(cu, location);
      if (reanchored != null) {
        return reanchored;
      }
    }

    return location;
  }

  /**
   * Resolves the source file recorded for a class and remembers the result.
   *
   * @param className the fully qualified class name
   * @return the source file path, or {@code null} when unknown
   */
  public String resolveSourceFile(String className) {
    String file = SourceFileResolver.resolve(className, SourceFileResolver.JAVA_ONLY);
    SourcePathRegistry.addPath(file);

    return file;
  }

  /**
   * Redirects a live composite's location to its {@code getBoundComponent()} alias variable.
   *
   * @param cu the parsed file the change targets
   * @param composite the live composite whose class hierarchy must own the file's class
   * @param location the location pointing into the composite's own class
   * @return the alias location, or {@code null} when the file holds no matching composite alias
   */
  public SourceLocation redirectCompositeAlias(CompilationUnit cu, Component composite,
      SourceLocation location) {
    return redirectCompositeAlias(cu, ComponentTypeNames.of(composite.getClass()), location);
  }

  private SourceLocation redirectCompositeAlias(CompilationUnit cu, SourceLocation location) {
    String typeName = location.getSimpleTypeName();
    return typeName == null ? null : redirectCompositeAlias(cu, Set.of(typeName), location);
  }

  private SourceLocation redirectCompositeAlias(CompilationUnit cu, Set<String> acceptableTypeNames,
      SourceLocation location) {
    ClassOrInterfaceDeclaration classDecl =
        cu.findFirst(ClassOrInterfaceDeclaration.class).orElse(null);
    if (classDecl == null || !acceptableTypeNames.contains(classDecl.getNameAsString())
        || !AstFinder.extendsComposite(classDecl)) {
      return null;
    }

    VariableDeclarator alias = AstFinder.findBoundComponentAlias(cu).orElse(null);
    Range aliasRange = alias == null ? null : alias.getRange().orElse(null);
    if (aliasRange == null) {
      return null;
    }

    String aliasType = alias.getType().asString();
    int generic = aliasType.indexOf('<');
    if (generic > 0) {
      aliasType = aliasType.substring(0, generic);
    }

    return new SourceLocation(location.getFile(), aliasRange.begin.line,
        location.getDeclaringClass(), alias.getNameAsString(), aliasType);
  }

  private SourceLocation reanchorByDeclaration(CompilationUnit cu, SourceLocation location) {
    String variableName = location.getVariableName();
    String typeName = location.getSimpleTypeName();
    if (variableName == null || variableName.isEmpty() || typeName == null || typeName.isEmpty()) {
      return location;
    }

    List<Range> matches = new ArrayList<>();
    for (VariableDeclarator varDecl : cu.findAll(VariableDeclarator.class)) {
      if (variableName.equals(varDecl.getNameAsString()) && AstFinder.matchesType(varDecl.getType(),
          varDecl.getInitializer().orElse(null), typeName)) {
        varDecl.getRange().ifPresent(matches::add);
      }
    }

    // The stored line still hitting the declaration means nothing moved; more than one candidate
    // means guessing, and guessing writes into the wrong component
    int line = location.getLine();
    boolean anchored =
        matches.stream().anyMatch(range -> range.begin.line <= line && range.end.line >= line);
    if (anchored || matches.size() != 1) {
      return location;
    }

    return new SourceLocation(location.getFile(), matches.get(0).begin.line,
        location.getDeclaringClass(), variableName, location.getComponentType());
  }
}
