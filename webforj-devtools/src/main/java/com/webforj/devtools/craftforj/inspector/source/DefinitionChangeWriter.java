package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.contribution.content.IconContribution;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator.GeneratorContext;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerators;
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Writes setter-style changes at the component's definition site.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
class DefinitionChangeWriter implements ChangeWriter {

  private final FeatureHandlerRegistry registry;
  private final TargetResolver targetResolver;
  private final List<ModificationStrategy> strategies;

  DefinitionChangeWriter(FeatureHandlerRegistry registry, TargetResolver targetResolver,
      List<ModificationStrategy> strategies) {
    this.registry = registry;
    this.targetResolver = targetResolver;
    this.strategies = strategies;
  }

  @Override
  public boolean claims(ChangeRequest change, WriteContext context) {
    return !ChangeWriter.isParentScoped(registry, change);
  }

  @Override
  public void write(CompilationUnit cu, List<ChangeRequest> changes, WriteContext context) {
    if (changes.isEmpty()) {
      return;
    }

    ChangeRequest first = changes.get(0);
    Component component = ComponentLocator.findById(first.getComponentId()).orElse(null);

    SourceLocation sourceLocation = targetResolver.resolve(component, first.getSource());
    if (sourceLocation == null || !sourceLocation.hasBasicInfo()) {
      throw new SourceModificationException("Source location not found");
    }

    // A composite's features live on its bound component: inside the composite's own file the
    // setters must target the variable holding getBoundComponent(), never the composite's
    // constructor line. Without a resolvable variable the bound-component strategy still applies.
    Component bound = targetResolver.unwrapBoundComponent(component);
    if (bound != component) {
      SourceLocation boundLocation = targetResolver.resolve(bound, first.getSource());
      if (boundLocation != null && boundLocation.hasBasicInfo()
          && boundLocation.getVariableName() != null && !boundLocation.getVariableName().isEmpty()
          && Objects.equals(Path.of(boundLocation.getFile()).normalize(),
              Path.of(sourceLocation.getFile()).normalize())) {
        sourceLocation = boundLocation;
        component = bound;
      } else {
        // Composite creates its bound component inside super(), so the recorded frame is the
        // constructor signature line and never the alias declaration; find the alias in the AST
        SourceLocation aliasLocation =
            targetResolver.redirectCompositeAlias(cu, component, sourceLocation);
        if (aliasLocation != null) {
          sourceLocation = aliasLocation;
          component = bound;
        }
      }
    }

    // A destroyed component only leaves the client's stored location behind. Re-derive the target
    // from the AST so a composite still writes on its bound-component alias and a shifted file
    // still finds the declaration by variable name and type.
    if (component == null) {
      sourceLocation = targetResolver.reanchorDestroyedLocation(cu, sourceLocation);
    }

    int lineNumber = sourceLocation.getLine();
    String typeNameFqn = sourceLocation.getComponentType();
    String typeName = sourceLocation.getSimpleTypeName();
    String variableName = sourceLocation.getVariableName();

    TargetContext target = new TargetContext(lineNumber, typeName);
    if (component != null) {
      target.setAcceptableTypes(ComponentTypeNames.of(component.getClass()));
    }

    // Generate all source changes first. Icon changes rewrite the icon expression in place and
    // never go through the setter strategies.
    Map<ChangeRequest, SourceChange> generated = new LinkedHashMap<>();
    for (ChangeRequest change : changes) {
      FeatureHandler handler = registry.getHandler(change.getFeatureType())
          .orElseThrow(() -> new SourceModificationException(
              "No handler found for feature type: " + change.getFeatureType()));

      if (handler instanceof IconContribution) {
        context.getRequiredImports()
            .addAll(IconExpressionRewriter.rewrite(cu, target, change.getValue()));
        continue;
      }

      SourceChange sourceChange =
          generateSourceChange(cu, change, handler, target, variableName, component);
      if (sourceChange != null) {
        generated.put(change, sourceChange);
        // A re-anchored alias type is a simple name; only fully qualified names are importable
        if (typeNameFqn != null && typeNameFqn.contains(".")) {
          context.getRequiredImports().add(typeNameFqn);
        }
        context.getRequiredImports().addAll(sourceChange.getImports());
      }
    }

    if (generated.isEmpty()) {
      return;
    }

    ModificationContext modification =
        new ModificationContext(target, variableName, new ArrayList<>(generated.values()));

    // Find and apply the appropriate strategy
    for (ModificationStrategy strategy : strategies) {
      if (strategy.canHandle(cu, target)) {
        strategy.apply(cu, modification);
        // The update step reports which computed argument it overwrote; hand that to the results
        for (Map.Entry<ChangeRequest, SourceChange> entry : generated.entrySet()) {
          String computed = entry.getValue().getReplacedComputedExpression();
          if (computed != null) {
            context.getReplacedExpressions().put(entry.getKey(), computed);
          }
        }
        return;
      }
    }

    throw new SourceModificationException("Cannot modify " + typeName + " at line " + lineNumber
        + ". No suitable modification strategy found.");
  }

  private SourceChange generateSourceChange(CompilationUnit cu, ChangeRequest change,
      FeatureHandler handler, TargetContext target, String variableName, Component component) {
    String methodName = handler.getSourceMethodName(change.getPropertyName());
    SourceGenerator generator = SourceGenerators.select(handler, component);

    // Transform value for source generation (handlers like KeyValue wrap value with key)
    Object sourceValue = handler.getSourceValue(change.getProperty());
    FeatureProperty sourceProperty =
        FeatureProperty.builder(change.getPropertyName(), change.getFeatureType())
            .javaType(change.getProperty().getJavaType()).value(sourceValue).build();

    SourceChange sourceChange =
        generator.generate(new GeneratorContext(methodName, sourceProperty));
    String accessor = handler.getSourceAccessor();

    // If generator returns null (e.g., empty list), remove the method call
    if (sourceChange == null) {
      if (variableName != null) {
        AstModifier.removeMethodCall(cu, variableName, methodName, accessor);
      } else if (AstFinder.usesBoundComponentPattern(cu, target)) {
        AstModifier.removeBoundComponentMethodCall(cu, methodName, accessor);
      }

      return null;
    }

    if (accessor != null) {
      sourceChange = sourceChange.withAccessor(accessor);
    }

    return sourceChange;
  }
}
