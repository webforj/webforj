package com.webforj.rewrite.v26;

import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.java.ChangeMethodName;
import org.openrewrite.java.JavaIsoVisitor;
import org.openrewrite.java.MethodMatcher;
import org.openrewrite.java.tree.Expression;
import org.openrewrite.java.tree.J;
import org.openrewrite.java.tree.JavaType;
import org.openrewrite.java.tree.TypeUtils;
import org.openrewrite.marker.SearchResult;

/**
 * Migrates {@code setText} calls whose argument is a string literal wrapped in {@code <html>} to
 * {@code setHtml}.
 *
 * <p>
 * Passing {@code <html>} content to a text setter is a deprecated opt-in to HTML rendering that
 * will be removed in webforJ 27. A string literal is hardcoded, trusted content, so such a call is
 * rewritten to {@code setHtml} when the receiver supports it. When the receiver has no
 * {@code setHtml}, or when the argument is not a literal, the call is flagged for manual handling
 * instead.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class MigrateHtmlInSetText extends Recipe {

  private static final String SET_TEXT_PATTERN = "com.webforj..* setText(java.lang.String)";
  private static final MethodMatcher SET_TEXT = new MethodMatcher(SET_TEXT_PATTERN, true);
  private static final String HAS_HTML = "com.webforj.concern.HasHtml";

  @Override
  public String getDisplayName() {
    return "Migrate <html> content from setText to setHtml";
  }

  @Override
  public String getDescription() {
    return "Rewrites setText calls whose argument is a string literal starting with <html> to "
        + "setHtml when the receiver supports it, since the legacy HTML in setText behavior is "
        + "deprecated and will be removed in webforJ 27. Calls that cannot be rewritten safely are "
        + "flagged for manual handling.";
  }

  @Override
  public TreeVisitor<?, ExecutionContext> getVisitor() {
    return new JavaIsoVisitor<ExecutionContext>() {

      @Override
      public J.MethodInvocation visitMethodInvocation(J.MethodInvocation method,
          ExecutionContext ctx) {
        J.MethodInvocation invocation = super.visitMethodInvocation(method, ctx);
        if (!SET_TEXT.matches(invocation) || invocation.getArguments().size() != 1) {
          return invocation;
        }

        Expression argument = invocation.getArguments().get(0);
        if (!(argument instanceof J.Literal literal) || !(literal.getValue() instanceof String text)
            || !text.trim().startsWith("<html>")) {
          return invocation;
        }

        if (!receiverHasSetHtml(invocation)) {
          return SearchResult.found(invocation,
              "webforJ 26: setText with <html> is deprecated and this type has no setHtml, handle "
                  + "manually. Removed in webforJ 27.");
        }

        return (J.MethodInvocation) new ChangeMethodName(SET_TEXT_PATTERN, "setHtml", true, null)
            .getVisitor().visitNonNull(invocation, ctx, getCursor().getParentOrThrow());
      }

      private boolean receiverHasSetHtml(J.MethodInvocation invocation) {
        Expression select = invocation.getSelect();
        JavaType type = null;
        if (select != null) {
          type = select.getType();
        } else if (invocation.getMethodType() != null) {
          type = invocation.getMethodType().getDeclaringType();
        }

        return TypeUtils.isAssignableTo(HAS_HTML, type);
      }
    };
  }
}
