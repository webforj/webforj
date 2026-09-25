package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.contribution.content.IconContribution;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator.GeneratorContext;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerators;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.TargetResolver;
import com.webforj.devtools.craftforj.source.model.ModificationContext;
import com.webforj.devtools.craftforj.source.model.SourceChange;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.model.TargetContext;
import com.webforj.devtools.craftforj.source.parser.TypeReferences;
import com.webforj.devtools.craftforj.source.strategy.ModificationStrategy;
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
      throw new SourceModificationException("The source location for this component was not found");
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
    final String typeNameFqn = sourceLocation.getComponentType();
    String typeName = sourceLocation.getSimpleTypeName();
    final String variableName = sourceLocation.getVariableName();

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
        IconExpressionRewriter.rewrite(cu, target, change.getValue(), context.getRequiredImports());
        continue;
      }

      SourceChange sourceChange = generateSourceChange(change, handler, component,
          resolveComponentType(component, sourceLocation.getComponentType()));
      if (sourceChange != null) {
        requireConsistentAliases(generated.values(), sourceChange);
        generated.put(change, sourceChange);
        TypeReferences.bind(cu, sourceChange.getArguments(), sourceChange.getImports(),
            context.getRequiredImports());
      }
    }

    if (generated.isEmpty()) {
      return;
    }

    ModificationContext modification =
        new ModificationContext(target, variableName, new ArrayList<>(generated.values()));
    if (typeNameFqn != null && typeNameFqn.contains(".")
        && generated.values().stream().anyMatch(change -> !change.isRemoval())) {
      modification.setDeclarationType(
          TypeReferences.resolveType(cu, typeNameFqn, context.getRequiredImports(), true));
    }

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

    throw new SourceModificationException("No " + typeName + " is declared at line " + lineNumber
        + ". The source changed since the application was compiled. "
        + "Reload the application before saving.");
  }

  private SourceChange generateSourceChange(ChangeRequest change, FeatureHandler handler,
      Component component, Class<?> componentType) {
    String methodName = handler.getSourceMethodName(change.getPropertyName());
    SourceGenerator generator = SourceGenerators.select(handler, component);

    // Transform value for source generation (handlers like KeyValue wrap value with key)
    Object sourceValue = handler.getSourceValue(change.getProperty());
    FeatureProperty sourceProperty =
        FeatureProperty.builder(change.getPropertyName(), change.getFeatureType())
            .javaType(change.getProperty().getJavaType()).value(sourceValue).build();

    GeneratorContext generatorContext = new GeneratorContext(methodName, sourceProperty);
    SourceChange sourceChange = generator.generate(generatorContext);
    String accessor = handler.getSourceAccessor();

    // Removal uses the same target strategy as insertion and replacement.
    if (sourceChange == null) {
      sourceChange = generator.createRemoval(generatorContext);
    }

    if (accessor != null) {
      sourceChange = sourceChange.withAccessor(accessor);
    }

    return sourceChange.withMethodExpansions(handler.getSourceMethodExpansions(componentType))
        .withPropertyName(change.getPropertyName());
  }

  private Class<?> resolveComponentType(Component component, String className) {
    if (component != null) {
      return component.getClass();
    }
    if (className == null) {
      return null;
    }
    Class<?> type = tryLoad(className, Thread.currentThread().getContextClassLoader());
    return type != null ? type : tryLoad(className, getClass().getClassLoader());
  }

  private static Class<?> tryLoad(String className, ClassLoader loader) {
    if (loader == null) {
      return null;
    }
    try {
      return Class.forName(className, false, loader);
    } catch (ClassNotFoundException | LinkageError e) {
      return null;
    }
  }

  private void requireConsistentAliases(java.util.Collection<SourceChange> previous,
      SourceChange current) {
    for (SourceChange other : previous) {
      boolean sameProperty = other.getMethodName().equals(current.getMethodName())
          || other.getMethodExpansions().getOrDefault(current.getMethodName(), List.of())
              .equals(List.of(other.getMethodName()))
          || current.getMethodExpansions().getOrDefault(other.getMethodName(), List.of())
              .equals(List.of(current.getMethodName()));
      if (sameProperty && Objects.equals(other.getAccessor(), current.getAccessor())
          && Objects.equals(other.getMatchKey(), current.getMatchKey())
          && (other.isRemoval() != current.isRemoval()
              || !other.getArguments().equals(current.getArguments()))) {
        throw new SourceModificationException("Conflicting edits target the same source property: '"
            + other.getMethodName() + "' and '" + current.getMethodName() + "'");
      }
    }
  }
}
