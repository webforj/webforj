package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.contribution.LayoutItemContribution;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.TargetResolver;
import com.webforj.devtools.craftforj.source.model.ModificationContext;
import com.webforj.devtools.craftforj.source.model.SourceChange;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.model.TargetContext;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.parser.AstModifier;
import com.webforj.devtools.craftforj.source.parser.TypeReferences;
import com.webforj.devtools.craftforj.source.strategy.ModificationStrategy;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import com.webforj.devtools.craftforj.utilities.ComponentTypeNames;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

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
    Component parentComponent = targetResolver.resolveParentComponent(first.getParentId());
    SourceLocation parentLocation =
        targetResolver.resolve(parentComponent, first.getParentSource());
    if (parentLocation == null || !parentLocation.hasBasicInfo()) {
      throw new SourceModificationException(
          "The source location for the parent layout was not found");
    }

    if (parentComponent == null) {
      parentLocation = targetResolver.reanchorDestroyedLocation(cu, parentLocation);
    }

    // The child decides the item variable referenced in the generated calls
    Component component = ComponentLocator.findById(first.getComponentId()).orElse(null);
    SourceLocation childLocation = targetResolver.resolve(component, first.getSource());
    if (childLocation == null || !childLocation.hasBasicInfo()) {
      throw new SourceModificationException("The source location for this component was not found");
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
    VariableDeclarator itemDeclaration = resolveItemDeclaration(cu, childLocation, itemVar);
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
        sourceChange = SourceChange.builder()
            .removeMethodCall(itemHandler.getSourceMethodName(change.getPropertyName()))
            .itemRef(itemVar, itemHandler.getItemPosition(), itemHandler.getItemCallArgumentCount())
            .build();
      }

      sourceChange = sourceChange.withPropertyName(change.getPropertyName());
      sourceChange.setItemDeclaration(itemDeclaration);
      sourceChanges.add(sourceChange);
      TypeReferences.bind(cu, sourceChange.getArguments(), sourceChange.getImports(),
          context.getRequiredImports());
    }

    if (sourceChanges.isEmpty()) {
      return;
    }

    ModificationContext modification =
        new ModificationContext(parentTarget, parentVar, sourceChanges);
    String parentType = parentLocation.getComponentType();
    if (parentType != null && parentType.contains(".")
        && sourceChanges.stream().anyMatch(change -> !change.isRemoval())) {
      modification.setDeclarationType(
          TypeReferences.resolveType(cu, parentType, context.getRequiredImports(), true));
    }
    for (ModificationStrategy strategy : strategies) {
      if (strategy.canHandle(cu, parentTarget)) {
        strategy.apply(cu, modification);
        return;
      }
    }

    throw new SourceModificationException(
        "No " + parentTarget.getTypeName() + " is declared at line " + parentTarget.getLineNumber()
            + ". The source changed since the application was compiled. "
            + "Reload the application before saving.");
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
    String typeName = childLocation.getComponentType();
    String declarationType = typeName != null && typeName.contains(".")
        ? TypeReferences.resolveType(cu, typeName, requiredImports, true)
        : childTarget.getTypeName();

    String extracted = AstFinder.findInlineCreationAt(cu, childTarget)
        .map(expr -> AstModifier.extractToVariable(expr, declarationType))
        .orElseGet(() -> AstFinder.findFactoryMethodAt(cu, childTarget)
            .map(expr -> AstModifier.extractToVariable(expr, declarationType)).orElse(null));

    if (extracted == null) {
      throw new SourceModificationException(
          "Cannot determine a variable name for the layout item at line "
              + childLocation.getLine());
    }

    return extracted;
  }

  private VariableDeclarator resolveItemDeclaration(CompilationUnit cu, SourceLocation location,
      String name) {
    return cu.findAll(VariableDeclarator.class).stream()
        .filter(variable -> variable.getNameAsString().equals(name))
        .filter(variable -> variable.getRange()
            .or(() -> variable.getInitializer().flatMap(Node::getRange))
            .map(range -> range.begin.line <= location.getLine()
                && range.end.line >= location.getLine())
            .orElse(false))
        .findFirst()
        .orElseThrow(() -> new SourceModificationException(
            "No layout item '" + name + "' is declared at line " + location.getLine()
                + ". The source changed since the application was compiled. "
                + "Reload the application before saving."));
  }

}
