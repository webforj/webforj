package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.inspector.source.model.FilePatch;
import com.webforj.devtools.craftforj.inspector.source.parser.ImportWriter;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.inspector.source.parser.StatementWrapper;
import com.webforj.devtools.craftforj.inspector.source.strategy.BoundComponentStrategy;
import com.webforj.devtools.craftforj.inspector.source.strategy.FactoryMethodStrategy;
import com.webforj.devtools.craftforj.inspector.source.strategy.FieldDeclarationStrategy;
import com.webforj.devtools.craftforj.inspector.source.strategy.InlineCreationStrategy;
import com.webforj.devtools.craftforj.inspector.source.strategy.LocalVariableStrategy;
import com.webforj.devtools.craftforj.inspector.source.strategy.ModificationStrategy;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Service for modifying Java source code based on property changes.
 *
 * <p>
 * This service takes property changes from the craftforJ inspector, groups them per file, and runs
 * them through an ordered list of {@link ChangeWriter} instances. Each writer owns one kind of
 * change, so new kinds plug in as standalone writers.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SourceCodeModifier {
  private static final System.Logger LOGGER = System.getLogger(SourceCodeModifier.class.getName());

  private final FeatureHandlerRegistry registry;
  private final SourceParserService parserService;
  private final TargetResolver targetResolver;
  private final UsagePlanner usagePlanner;
  private final List<ChangeWriter> writers;

  private static class GroupResult {

    private final Map<Path, List<ChangeRequest>> grouped;
    private final Map<ChangeRequest, String> failures;
    private final Map<ChangeRequest, UsagePlanner.UsagePlan> plans;
    private final Map<ChangeRequest, String> resolvedTargets;

    GroupResult(Map<Path, List<ChangeRequest>> grouped, Map<ChangeRequest, String> failures,
        Map<ChangeRequest, UsagePlanner.UsagePlan> plans,
        Map<ChangeRequest, String> resolvedTargets) {
      this.grouped = grouped;
      this.failures = failures;
      this.plans = plans;
      this.resolvedTargets = resolvedTargets;
    }

    Map<Path, List<ChangeRequest>> getGrouped() {
      return grouped;
    }

    Map<ChangeRequest, String> getFailures() {
      return failures;
    }

    Map<ChangeRequest, UsagePlanner.UsagePlan> getPlans() {
      return plans;
    }

    Map<ChangeRequest, String> getResolvedTargets() {
      return resolvedTargets;
    }
  }

  private static class FileOutcome {

    private final Map<String, String> errors;
    private final String original;
    private final String modified;
    private final Map<ChangeRequest, String> replacedExpressions;

    FileOutcome(Map<String, String> errors, String original, String modified,
        Map<ChangeRequest, String> replacedExpressions) {
      this.errors = errors;
      this.original = original;
      this.modified = modified;
      this.replacedExpressions = replacedExpressions;
    }

    Map<String, String> getErrors() {
      return errors;
    }

    String getOriginal() {
      return original;
    }

    String getModified() {
      return modified;
    }

    Map<ChangeRequest, String> getReplacedExpressions() {
      return replacedExpressions;
    }
  }

  /**
   * Creates a new SourceCodeModifier.
   *
   * @param registry the contribution registry
   * @param parserService the parser service
   */
  public SourceCodeModifier(FeatureHandlerRegistry registry, SourceParserService parserService) {
    this.registry = registry;
    this.parserService = parserService;
    this.targetResolver = new TargetResolver(parserService);
    this.usagePlanner = new UsagePlanner(registry, parserService, targetResolver);
    this.writers = createWriters();
  }

  /**
   * Validates changes without writing to files.
   *
   * @param changes the changes to validate
   * @return results for each change (success or failure with error message)
   */
  public List<ChangeResult> preview(List<ChangeRequest> changes) {
    return processChanges(changes, true);
  }

  /**
   * Applies changes to source files.
   *
   * @param changes the changes to apply
   * @return results for each change (success or failure with error message)
   */
  public List<ChangeResult> apply(List<ChangeRequest> changes) {
    return processChanges(changes, false);
  }

  /**
   * Produces the before and after content of every file the changes would touch.
   *
   * <p>
   * Nothing is written. Files whose changes all failed are left out, so an empty result means there
   * is nothing to show rather than an empty diff.
   * </p>
   *
   * @param changes the changes to preview
   * @return one patch per affected file
   */
  public List<FilePatch> previewPatches(List<ChangeRequest> changes) {
    GroupResult groupResult = groupByFile(changes, true);
    List<FilePatch> patches = new ArrayList<>();

    for (Map.Entry<Path, List<ChangeRequest>> entry : groupResult.getGrouped().entrySet()) {
      Path file = entry.getKey();
      try {
        FileOutcome outcome = processFile(file, entry.getValue(), groupResult.getPlans(), true);
        if (outcome.getModified() != null) {
          patches.add(new FilePatch(file.toString(), outcome.getOriginal(), outcome.getModified()));
        }
      } catch (Exception e) {
        LOGGER.log(System.Logger.Level.DEBUG, () -> "Failed to preview patch for file: " + file, e);
      }
    }

    return patches;
  }

  private List<ChangeResult> processChanges(List<ChangeRequest> changes, boolean dryRun) {
    List<ChangeResult> results = new ArrayList<>();

    GroupResult groupResult = groupByFile(changes, dryRun);

    for (Map.Entry<ChangeRequest, String> entry : groupResult.getFailures().entrySet()) {
      ChangeRequest req = entry.getKey();
      results.add(ChangeResult.failure(req.getComponentId(), req.getProperty(), entry.getValue()));
    }

    for (Map.Entry<Path, List<ChangeRequest>> entry : groupResult.getGrouped().entrySet()) {
      try {
        FileOutcome outcome =
            processFile(entry.getKey(), entry.getValue(), groupResult.getPlans(), dryRun);
        Map<String, String> errors = outcome.getErrors();
        for (ChangeRequest change : entry.getValue()) {
          String error = errors.get(change.getComponentId());
          ChangeResult result;
          if (error != null) {
            result = ChangeResult.failure(change.getComponentId(), change.getProperty(), error);
          } else {
            UsagePlanner.UsagePlan plan = groupResult.getPlans().get(change);
            SourceLocation source = plan != null ? plan.getUsageLocation()
                : change.getSource() != null ? change.getSource()
                    : new SourceLocation(entry.getKey().toString(), null, null, null, null);
            result = ChangeResult.success(change.getComponentId(), change.getProperty(), source);
            result.setReplacedExpression(outcome.getReplacedExpressions().get(change));
          }
          result.setResolvedTarget(groupResult.getResolvedTargets().get(change));
          results.add(result);
        }
      } catch (Exception e) {
        LOGGER.log(System.Logger.Level.DEBUG,
            () -> "Failed to process changes for file: " + entry.getKey(), e);
        for (ChangeRequest change : entry.getValue()) {
          results.add(
              ChangeResult.failure(change.getComponentId(), change.getProperty(), errorMessage(e)));
        }
      }
    }

    return results;
  }

  private List<ChangeWriter> createWriters() {
    // Order matters: usage-site rewrites run before definition writes so shared components are
    // never touched at their definition when a usage plan claimed the change first
    List<ModificationStrategy> strategies = createStrategies();
    return List.of(new UsageChangeWriter(),
        new DefinitionChangeWriter(registry, targetResolver, strategies),
        new LayoutItemChangeWriter(registry, targetResolver, strategies));
  }

  private List<ModificationStrategy> createStrategies() {
    // Order matters: more specific strategies first
    // LocalVariableStrategy before BoundComponentStrategy because local variables
    // in Composite classes should use the variable name, not getBoundComponent()
    return List.of(new FieldDeclarationStrategy(), new InlineCreationStrategy(),
        new FactoryMethodStrategy(), new LocalVariableStrategy(), new BoundComponentStrategy());
  }

  private GroupResult groupByFile(List<ChangeRequest> changes, boolean dryRun) {
    Map<Path, List<ChangeRequest>> grouped = new LinkedHashMap<>();
    Map<ChangeRequest, String> failures = new LinkedHashMap<>();
    Map<ChangeRequest, UsagePlanner.UsagePlan> plans = new LinkedHashMap<>();
    Map<ChangeRequest, String> resolvedTargets = new LinkedHashMap<>();

    for (ChangeRequest change : changes) {
      try {
        if (change.isUsageTargeted() && !ChangeWriter.isParentScoped(registry, change)) {
          UsagePlanner.UsagePlan plan = usagePlanner.plan(change);
          if (plan != null) {
            plans.put(change, plan);
            resolvedTargets.put(change, ChangeRequest.TARGET_USAGE);
            grouped.computeIfAbsent(Path.of(plan.getUsageLocation().getFile()).normalize(),
                k -> new ArrayList<>()).add(change);
            continue;
          }

          // Apply must never silently land in the shared definition the client did not confirm.
          // Preview falls through so the client learns the resolved target before writing.
          if (!dryRun) {
            failures.put(change,
                "The property cannot be changed at the usage site. Preview again to refresh the "
                    + "target.");
            continue;
          }
          resolvedTargets.put(change, ChangeRequest.TARGET_DEFINITION);
        }

        SourceLocation sourceLocation;
        if (ChangeWriter.isParentScoped(registry, change)) {
          // Parent-scoped changes are written into the parent layout's source file
          sourceLocation = targetResolver.resolveParent(change);
          if (sourceLocation == null || sourceLocation.getFile() == null) {
            failures.put(change, "Parent layout source file not found");
            continue;
          }
        } else {
          Component component = ComponentLocator.findById(change.getComponentId()).orElse(null);
          sourceLocation = targetResolver.resolve(component, change.getSource());
        }

        if (sourceLocation == null || sourceLocation.getFile() == null) {
          failures.put(change, "Source file not found");
          continue;
        }

        Path path = Path.of(sourceLocation.getFile());
        grouped.computeIfAbsent(path, k -> new ArrayList<>()).add(change);
      } catch (Exception e) {
        LOGGER.log(System.Logger.Level.DEBUG, "Failed to group change by file", e);
        failures.put(change, errorMessage(e));
      }
    }

    return new GroupResult(grouped, failures, plans, resolvedTargets);
  }

  private FileOutcome processFile(Path file, List<ChangeRequest> changes,
      Map<ChangeRequest, UsagePlanner.UsagePlan> plans, boolean dryRun) throws IOException {
    String originalContent = Files.readString(file);
    CompilationUnit cu = parserService.parseWithLexicalPreservation(originalContent)
        .orElseThrow(() -> new SourceModificationException("Failed to parse source file: " + file));

    // Group changes by componentId to process all changes for a component together
    Map<String, List<ChangeRequest>> byComponent = changes.stream().collect(Collectors
        .groupingBy(ChangeRequest::getComponentId, LinkedHashMap::new, Collectors.toList()));

    Map<String, String> errors = new LinkedHashMap<>();
    Set<String> requiredImports = new LinkedHashSet<>();
    WriteContext context = new WriteContext(plans, requiredImports);
    boolean anySucceeded = false;

    for (Map.Entry<String, List<ChangeRequest>> entry : byComponent.entrySet()) {
      try {
        List<ChangeRequest> remaining = new ArrayList<>(entry.getValue());
        for (ChangeWriter writer : writers) {
          List<ChangeRequest> claimed =
              remaining.stream().filter(change -> writer.claims(change, context)).toList();
          if (!claimed.isEmpty()) {
            writer.write(cu, claimed, context);
            remaining.removeAll(claimed);
          }
        }
        anySucceeded = true;
      } catch (Exception e) {
        LOGGER.log(System.Logger.Level.DEBUG,
            () -> "Failed to apply changes for component: " + entry.getKey(), e);
        errors.put(entry.getKey(), errorMessage(e));
      }
    }

    if (!anySucceeded) {
      return new FileOutcome(errors, originalContent, null, context.getReplacedExpressions());
    }

    String modified = StatementWrapper.wrap(originalContent,
        ImportWriter.sync(parserService.print(cu), requiredImports, requiredImports));
    if (!dryRun) {
      Files.writeString(file, modified);
    }

    return new FileOutcome(errors, originalContent, modified, context.getReplacedExpressions());
  }

  private static String errorMessage(Exception e) {
    String message = e.getMessage();
    return message != null && !message.isBlank() ? message : e.getClass().getSimpleName();
  }
}
