package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.element.ElementComposite;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.parser.AstFinder;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
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
class TargetResolver {

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

  TargetResolver(SourceParserService parserService) {
    this.parserService = parserService;
    this.reanchorRules = List.of(this::redirectCompositeAlias, this::reanchorByDeclaration);
  }

  /**
   * Resolves the source location for a component, preferring runtime information.
   *
   * @param component the live component, or {@code null} when it was destroyed
   * @param fallback the client-stored location used when the component is gone
   * @return the resolved location, or {@code null} when none can be trusted
   */
  SourceLocation resolve(Component component, SourceLocation fallback) {
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
   * Resolves the source location of the change's parent layout.
   *
   * @param change the change whose parent is resolved
   * @return the parent location, or {@code null} when none can be trusted
   */
  SourceLocation resolveParent(ChangeRequest change) {
    return resolve(resolveParentComponent(change), change.getParentSource());
  }

  /**
   * Resolves the parent component the change is scoped to.
   *
   * @param change the change whose parent is resolved
   * @return the parent component, or {@code null} when it is gone
   */
  Component resolveParentComponent(ChangeRequest change) {
    Component parent =
        change.getParentId() != null ? ComponentLocator.findById(change.getParentId()).orElse(null)
            : null;

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
  Component unwrapBoundComponent(Component component) {
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
  SourceLocation reanchorDestroyedLocation(CompilationUnit cu, SourceLocation location) {
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
  String resolveSourceFile(String className) {
    String file = SourceFileResolver.resolve(className, SourceFileResolver.JAVA_ONLY);
    SourcePathRegistry.record(file);

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
  SourceLocation redirectCompositeAlias(CompilationUnit cu, Component composite,
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
    if (alias == null || alias.getRange().isEmpty()) {
      return null;
    }

    String aliasType = alias.getType().asString();
    int generic = aliasType.indexOf('<');
    if (generic > 0) {
      aliasType = aliasType.substring(0, generic);
    }

    return new SourceLocation(location.getFile(), alias.getRange().get().begin.line,
        location.getDeclaringClass(), alias.getNameAsString(), aliasType);
  }

  private SourceLocation reanchorByDeclaration(CompilationUnit cu, SourceLocation location) {
    String variableName = location.getVariableName();
    String typeName = location.getSimpleTypeName();
    if (variableName == null || variableName.isEmpty() || typeName == null || typeName.isEmpty()) {
      return location;
    }

    List<VariableDeclarator> matches = new ArrayList<>();
    for (VariableDeclarator varDecl : cu.findAll(VariableDeclarator.class)) {
      if (varDecl.getRange().isPresent() && variableName.equals(varDecl.getNameAsString())
          && AstFinder.matchesType(varDecl.getType(), varDecl.getInitializer().orElse(null),
              typeName)) {
        matches.add(varDecl);
      }
    }

    // The stored line still hitting the declaration means nothing moved; more than one candidate
    // means guessing, and guessing writes into the wrong component
    int line = location.getLine();
    boolean anchored =
        matches.stream().anyMatch(varDecl -> varDecl.getRange().get().begin.line <= line
            && varDecl.getRange().get().end.line >= line);
    if (anchored || matches.size() != 1) {
      return location;
    }

    return new SourceLocation(location.getFile(), matches.get(0).getRange().get().begin.line,
        location.getDeclaringClass(), variableName, location.getComponentType());
  }
}
