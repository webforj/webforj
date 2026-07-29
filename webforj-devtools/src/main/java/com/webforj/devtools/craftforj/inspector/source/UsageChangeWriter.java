package com.webforj.devtools.craftforj.inspector.source;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.parser.UsageSiteRewriter;
import java.util.List;

/**
 * Rewrites planned usage-site calls of a shared component.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
class UsageChangeWriter implements ChangeWriter {

  @Override
  public boolean claims(ChangeRequest change, WriteContext context) {
    return context.getPlans().containsKey(change);
  }

  @Override
  public void write(CompilationUnit cu, List<ChangeRequest> changes, WriteContext context) {
    for (ChangeRequest change : changes) {
      UsagePlanner.UsagePlan plan = context.getPlans().get(change);
      String computed =
          UsageSiteRewriter.rewrite(cu, plan.getUsageLocation().getLine(), plan.getTraces(),
              change.getOriginalValue(), change.getValue(), change.getProperty().getJavaType());
      if (computed != null) {
        context.getReplacedExpressions().put(change, computed);
      }
    }
  }
}
