package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import java.util.List;

/**
 * Writes one kind of change into a parsed source file.
 *
 * <p>
 * Writers are consulted in a fixed order and each claims the changes it knows how to write, so a
 * new kind of change plugs in as a standalone writer instead of another branch in the pipeline.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
interface ChangeWriter {

  /**
   * Reports whether this writer handles the given change.
   *
   * @param change the change to route
   * @param context the shared per-file write state
   * @return {@code true} when this writer claims the change
   */
  boolean claims(ChangeRequest change, WriteContext context);

  /**
   * Writes the claimed changes of a single component into the parsed file.
   *
   * @param cu the parsed file to modify
   * @param changes the claimed changes, all belonging to one component
   * @param context the shared per-file write state
   */
  void write(CompilationUnit cu, List<ChangeRequest> changes, WriteContext context);

  /**
   * Reports whether the change writes into its parent layout's scope.
   *
   * @param registry the contribution registry
   * @param change the change to inspect
   * @return {@code true} when the change's feature is parent scoped
   */
  static boolean isParentScoped(FeatureHandlerRegistry registry, ChangeRequest change) {
    return registry.getHandler(change.getFeatureType()).map(FeatureHandler::isParentScoped)
        .orElse(false);
  }
}
