package com.webforj.devtools.craftforj.source;

import com.github.javaparser.ast.CompilationUnit;

/**
 * One change a feature makes to a parsed source file.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@FunctionalInterface
public interface SourceEdit {

  /**
   * Applies the change to the parsed file.
   *
   * @param cu the parsed file, set up for lexical preservation
   * @return {@code true} when the file changed and has to be printed
   */
  boolean apply(CompilationUnit cu);
}
