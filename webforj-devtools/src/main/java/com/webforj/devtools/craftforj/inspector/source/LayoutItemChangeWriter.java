package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.contribution.LayoutItemContribution;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import com.webforj.devtools.craftforj.inspector.source.parser.AstFinder;
import com.webforj.devtools.craftforj.inspector.source.parser.AstModifier;
import com.webforj.devtools.craftforj.inspector.source.strategy.ModificationStrategy;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import com.webforj.devtools.craftforj.utilities.ComponentTypeNames;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;

/**
 * Writes parent-scoped layout item changes into the parent layout's source file.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
class LayoutItemChangeWriter implements ChangeWriter {

  private final FeatureHandlerRegistry registry;
  private final TargetResolver targetResolver;
  private final List<ModificationStrategy> strategies;

  LayoutItemChangeWriter(FeatureHandlerRegistry registry, TargetResolver targetResolver,
      List<ModificationStrategy> strategies) {
    this.registry = registry;
    this.targetResolver = targetResolver;
    this.strategies = strategies;
  }

  @Override
  public boolean claims(ChangeRequest change, WriteContext context) {
    return ChangeWriter.isParentScoped(registry, change);
  }

  @Override
  public void write(CompilationUnit cu, List<ChangeRequest> changes, WriteContext context) {
    if (changes.isEmpty()) {
      return;
    }

    ChangeRequest first = changes.get(0);

    // The parent layout's source location decides where the calls are written
    SourceLocation parentLocation = targetResolver.resolveParent(first);
    if (parentLocation == null || !parentLocation.hasBasicInfo()) {
      throw new SourceModificationException("Parent layout source location not found");
    }

    Component parentComponent = targetResolver.resolveParentComponent(first);
    if (parentComponent == null) {
      parentLocation = targetResolver.reanchorDestroyedLocation(cu, parentLocation);
    }

    // The child decides the item variable referenced in the generated calls
    Component component = ComponentLocator.findById(first.getComponentId()).orElse(null);
    SourceLocation childLocation = targetResolver.resolve(component, first.getSource());
    if (childLocation == null || !childLocation.hasBasicInfo()) {
      throw new SourceModificationException("Source location not found");
    }

    if (component == null) {
      childLocation = targetResolver.reanchorDestroyedLocation(cu, childLocation);
    }

    if (!Objects.equals(Path.of(childLocation.getFile()).normalize(),
        Path.of(parentLocation.getFile()).normalize())) {
      throw new SourceModificationException(
          "Layout item properties require the item and its parent layout to be created in the "
              + "same file, but the item was created in "
              + Path.of(childLocation.getFile()).getFileName());
    }

    String itemVar = resolveItemVariable(cu, childLocation, context.getRequiredImports());
    TargetContext parentTarget =
        new TargetContext(parentLocation.getLine(), parentLocation.getSimpleTypeName());
    if (parentComponent != null) {
      parentTarget.setAcceptableTypes(ComponentTypeNames.of(parentComponent.getClass()));
    }
    String parentVar = parentLocation.getVariableName();

    List<SourceChange> sourceChanges = new ArrayList<>();
    for (ChangeRequest change : changes) {
      FeatureHandler handler = registry.getHandler(change.getFeatureType())
          .orElseThrow(() -> new SourceModificationException(
              "No handler found for feature type: " + change.getFeatureType()));

      if (!(handler instanceof LayoutItemContribution<?> itemHandler)) {
        throw new SourceModificationException(
            "Feature type is parent-scoped but not a layout item contribution: "
                + change.getFeatureType());
      }

      SourceChange sourceChange = itemHandler.buildItemSourceChange(change.getProperty(), itemVar);
      if (sourceChange == null) {
        removeItemCall(cu, parentTarget, parentVar, itemHandler, change, itemVar);
        continue;
      }

      sourceChanges.add(sourceChange);
      context.getRequiredImports().addAll(sourceChange.getImports());
    }

    if (sourceChanges.isEmpty()) {
      return;
    }

    ModificationContext modification =
        new ModificationContext(parentTarget, parentVar, sourceChanges);
    for (ModificationStrategy strategy : strategies) {
      if (strategy.canHandle(cu, parentTarget)) {
        strategy.apply(cu, modification);
        return;
      }
    }

    throw new SourceModificationException(
        "Cannot modify " + parentTarget.getTypeName() + " at line " + parentTarget.getLineNumber()
            + ". No suitable modification strategy found.");
  }

  private String resolveItemVariable(CompilationUnit cu, SourceLocation childLocation,
      Set<String> requiredImports) {
    String itemVar = childLocation.getVariableName();
    if (itemVar != null && !itemVar.isEmpty()) {
      return itemVar;
    }

    // Inline-created children get extracted to a variable so the parent call can reference them
    TargetContext childTarget =
        new TargetContext(childLocation.getLine(), childLocation.getSimpleTypeName());

    String extracted = AstFinder.findInlineCreationAt(cu, childTarget)
        .map(expr -> AstModifier.extractToVariable(expr, childTarget.getTypeName()))
        .orElseGet(() -> AstFinder.findFactoryMethodAt(cu, childTarget)
            .map(expr -> AstModifier.extractToVariable(expr, childTarget.getTypeName()))
            .orElse(null));

    if (extracted == null) {
      throw new SourceModificationException(
          "Cannot determine a variable name for the layout item at line "
              + childLocation.getLine());
    }

    if (childLocation.getComponentType() != null) {
      requiredImports.add(childLocation.getComponentType());
    }

    return extracted;
  }

  private void removeItemCall(CompilationUnit cu, TargetContext parentTarget, String parentVar,
      LayoutItemContribution<?> itemHandler, ChangeRequest change, String itemVar) {
    Predicate<MethodCallExpr> scopeMatcher = null;
    if (parentVar != null && !parentVar.isEmpty()) {
      scopeMatcher = mc -> AstModifier.isMethodCallOnVariable(mc, parentVar);
    } else if (AstFinder.usesBoundComponentPattern(cu, parentTarget)) {
      scopeMatcher = AstModifier::isMethodCallOnBoundComponent;
    }

    if (scopeMatcher != null) {
      AstModifier.removeItemCall(cu, scopeMatcher,
          itemHandler.getSourceMethodName(change.getPropertyName()), itemVar,
          itemHandler.getItemPosition(), itemHandler.getItemCallArgumentCount());
    }
  }
}
