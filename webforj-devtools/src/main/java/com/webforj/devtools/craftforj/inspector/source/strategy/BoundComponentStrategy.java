package com.webforj.devtools.craftforj.inspector.source.strategy;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import com.webforj.devtools.craftforj.inspector.source.parser.AstFinder;
import com.webforj.devtools.craftforj.inspector.source.parser.AstModifier;
import java.util.Optional;

/**
 * Strategy for Composite components that use getBoundComponent() pattern.
 *
 * <p>
 * This strategy handles classes that extend {@code Composite<T>} and access their bound component
 * directly via {@code getBoundComponent()} without assigning it to a variable.
 * </p>
 *
 * <p>
 * Example:
 * </p>
 *
 * <pre>
 * public class DrawerHeader extends Composite&lt;FlexLayout&gt; {
 *   public DrawerHeader() {
 *     getBoundComponent().setDirection(FlexDirection.COLUMN);
 *     getBoundComponent().setText("Hello"); // Added by this strategy
 *   }
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class BoundComponentStrategy implements ModificationStrategy {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean canHandle(CompilationUnit cu, TargetContext target) {
    return AstFinder.usesBoundComponentPattern(cu, target);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void apply(CompilationUnit cu, ModificationContext context) {
    Optional<ClassOrInterfaceDeclaration> classDecl =
        cu.findFirst(ClassOrInterfaceDeclaration.class);
    if (classDecl.isEmpty()) {
      return;
    }

    AstModifier.addSettersForBoundComponent(cu, classDecl.get(), context.getSourceChanges());
  }
}
