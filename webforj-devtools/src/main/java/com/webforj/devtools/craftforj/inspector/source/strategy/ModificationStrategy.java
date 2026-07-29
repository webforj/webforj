package com.webforj.devtools.craftforj.inspector.source.strategy;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;

/**
 * Strategy for modifying source code based on component creation pattern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public interface ModificationStrategy {

  /**
   * Checks if this strategy can handle the component at the given target.
   *
   * @param cu the compilation unit
   * @param target the target context identifying the component
   *
   * @return true if this strategy can handle it
   */
  boolean canHandle(CompilationUnit cu, TargetContext target);

  /**
   * Applies modifications to add setter calls.
   *
   * @param cu the compilation unit
   * @param context the modification context containing target, variable name, and source changes
   */
  void apply(CompilationUnit cu, ModificationContext context);
}
