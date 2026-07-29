package com.webforj.devtools.craftforj.inspector.source.strategy;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import com.webforj.devtools.craftforj.inspector.source.parser.AstFinder;
import com.webforj.devtools.craftforj.inspector.source.parser.AstModifier;
import java.util.Optional;

/**
 * Strategy for components created via factory methods (e.g., Icon.create()).
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class FactoryMethodStrategy implements ModificationStrategy {

  @Override
  public boolean canHandle(CompilationUnit cu, TargetContext target) {
    return AstFinder.findFactoryMethodAt(cu, target).isPresent();
  }

  @Override
  public void apply(CompilationUnit cu, ModificationContext context) {
    Optional<MethodCallExpr> factoryMethod = AstFinder.findFactoryMethodAt(cu, context.getTarget());
    if (factoryMethod.isEmpty()) {
      return;
    }

    AstModifier.extractToVariableAndAddSetters(factoryMethod.get(), context.getSourceChanges(),
        context.getTypeName());
  }
}
