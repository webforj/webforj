package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import com.webforj.devtools.craftforj.inspector.source.generator.ScalarSourceGenerator;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerators;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.inspector.source.parser.UsageSiteRewriter;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import com.webforj.devtools.craftforj.utilities.ComponentTypeNames;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

/**
 * Plans usage-site rewrites for changes that target where a shared component is used instead of
 * where it is defined.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
class UsagePlanner {
  private static final System.Logger LOGGER = System.getLogger(UsagePlanner.class.getName());

  private final FeatureHandlerRegistry registry;
  private final SourceParserService parserService;
  private final TargetResolver targetResolver;

  /**
   * The resolved usage location together with the traces to rewrite there.
   */
  static class UsagePlan {

    private final SourceLocation usageLocation;
    private final List<UsageSiteRewriter.Trace> traces;

    UsagePlan(SourceLocation usageLocation, List<UsageSiteRewriter.Trace> traces) {
      this.usageLocation = usageLocation;
      this.traces = traces;
    }

    SourceLocation getUsageLocation() {
      return usageLocation;
    }

    List<UsageSiteRewriter.Trace> getTraces() {
      return traces;
    }
  }

  UsagePlanner(FeatureHandlerRegistry registry, SourceParserService parserService,
      TargetResolver targetResolver) {
    this.registry = registry;
    this.parserService = parserService;
    this.targetResolver = targetResolver;
  }

  /**
   * Builds a usage plan for the change when its component is shared and traceable.
   *
   * @param change the usage-targeted change
   * @return the plan, or {@code null} when the definition site must be used instead
   */
  UsagePlan plan(ChangeRequest change) {
    Component component = ComponentLocator.findById(change.getComponentId()).orElse(null);
    if (component == null || change.getOriginalValue() == null) {
      return null;
    }

    FeatureHandler handler = registry.getHandler(change.getFeatureType()).orElse(null);
    if (handler == null || handler.getSourceAccessor() != null
        || !(SourceGenerators.select(handler, component) instanceof ScalarSourceGenerator)) {
      return null;
    }

    String methodName = handler.getSourceMethodName(change.getPropertyName());
    if (methodName == null) {
      return null;
    }

    SourcePoint creationPoint = ComponentSourceRegistry.getSourcePoint(component);
    if (creationPoint == null) {
      return null;
    }

    String creationFile = targetResolver.resolveSourceFile(creationPoint.className());
    if (creationFile == null) {
      return null;
    }

    SourcePoint usagePoint = null;
    String usageFile = null;
    List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);
    for (int i = 1; i < chain.size(); i++) {
      String file = targetResolver.resolveSourceFile(chain.get(i).className());
      if (file == null) {
        break;
      }
      if (!Path.of(file).normalize().equals(Path.of(creationFile).normalize())) {
        usagePoint = chain.get(i);
        usageFile = file;
        break;
      }
    }

    if (usagePoint == null) {
      return null;
    }

    try {
      CompilationUnit creationCu = parserService.parse(Path.of(creationFile)).orElse(null);
      if (creationCu == null) {
        return null;
      }

      TargetContext target =
          new TargetContext(creationPoint.lineNumber(), component.getClass().getSimpleName());
      target.setAcceptableTypes(ComponentTypeNames.of(component.getClass()));

      List<UsageSiteRewriter.Trace> traces =
          UsageSiteRewriter.trace(creationCu, target, methodName);
      if (traces.isEmpty()) {
        return null;
      }

      SourceLocation usageLocation = new SourceLocation(usageFile, usagePoint.lineNumber(),
          usagePoint.className(), null, component.getClass().getName());

      return new UsagePlan(usageLocation, traces);
    } catch (IOException e) {
      LOGGER.log(System.Logger.Level.DEBUG, "Failed to resolve usage plan", e);
      return null;
    }
  }
}
