package com.webforj.devtools.craftforj.inspector.source.strategy;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import com.webforj.devtools.craftforj.inspector.source.parser.AstFinder;
import com.webforj.devtools.craftforj.inspector.source.parser.AstModifier;
import java.util.Optional;

/**
 * Strategy for inline component creation (new X()).
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class InlineCreationStrategy implements ModificationStrategy {

  @Override
  public boolean canHandle(CompilationUnit cu, TargetContext target) {
    return AstFinder.findInlineCreationAt(cu, target).isPresent();
  }

  @Override
  public void apply(CompilationUnit cu, ModificationContext context) {
    Optional<ObjectCreationExpr> creation = AstFinder.findInlineCreationAt(cu, context.getTarget());
    if (creation.isEmpty()) {
      return;
    }

    AstModifier.extractToVariableAndAddSetters(creation.get(), context.getSourceChanges(),
        context.getTypeName());
  }
}
