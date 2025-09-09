package com.webforj.spring.security.evaluator;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import com.webforj.spring.security.annotation.RouteAccess;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.security.access.expression.SecurityExpressionRoot;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * Spring Security evaluator for {@link RouteAccess} annotation with SpEL support.
 *
 * <p>
 * This evaluator provides Spring Expression Language (SpEL) evaluation for {@code @RouteAccess}
 * annotations. It integrates with Spring Security to provide access to the current authentication
 * and user details.
 * </p>
 *
 * <p>
 * Available SpEL variables:
 * </p>
 * <ul>
 * <li>{@code authentication} - The current Spring Security Authentication object</li>
 * <li>{@code principal} - The authenticated principal</li>
 * <li>{@code routeClass} - The route component class being evaluated</li>
 * <li>{@code context} - The navigation context</li>
 * <li>{@code securityContext} - The route security context</li>
 * </ul>
 *
 * <p>
 * Available SpEL functions:
 * </p>
 * <ul>
 * <li>{@code hasRole(role)} - Check if user has specific role</li>
 * <li>{@code hasAnyRole(roles...)} - Check if user has any of the specified roles</li>
 * <li>{@code hasAuthority(authority)} - Check if user has specific authority</li>
 * <li>{@code hasAnyAuthority(authorities...)} - Check if user has any of the specified
 * authorities</li>
 * <li>{@code isAuthenticated()} - Check if user is authenticated</li>
 * <li>{@code isAnonymous()} - Check if user is anonymous</li>
 * </ul>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public class SpringRouteAccessEvaluator implements RouteSecurityEvaluator {
  private static final Logger logger = System.getLogger(SpringRouteAccessEvaluator.class.getName());
  private final ExpressionParser parser = new SpelExpressionParser();

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean supports(Class<?> routeClass) {
    return routeClass.isAnnotationPresent(RouteAccess.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {

    // @RouteAccess requires authentication
    if (!securityContext.isAuthenticated()) {
      return RouteAccessDecision.denyAuthentication();
    }

    RouteAccess annotation = routeClass.getAnnotation(RouteAccess.class);
    String expression = annotation.value();

    // Evaluate the expression
    return evaluateExpression(expression, routeClass, context, securityContext);
  }

  /**
   * Evaluates the access control expression using SpEL.
   *
   * @param expression the expression to evaluate
   * @param routeClass the route component class
   * @param context the navigation context
   * @param securityContext the security context
   * @return the access decision
   */
  private RouteAccessDecision evaluateExpression(String expression, Class<?> routeClass,
      NavigationContext context, RouteSecurityContext securityContext) {

    try {
      // Parse the expression
      Expression exp = parser.parseExpression(expression);

      // Create evaluation context
      EvaluationContext evalContext = createEvaluationContext(routeClass, context, securityContext);

      // Evaluate the expression
      Boolean result = exp.getValue(evalContext, Boolean.class);

      if (Boolean.TRUE.equals(result)) {
        return RouteAccessDecision.grant();
      } else {
        return RouteAccessDecision.denyPermissions("Access denied by expression: " + expression);
      }
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error evaluating SpEL expression: {0}", expression, e);
      return RouteAccessDecision.deny("Expression evaluation error");
    }
  }

  /**
   * Creates the evaluation context for SpEL expression evaluation.
   *
   * @param routeClass the route component class
   * @param context the navigation context
   * @param securityContext the security context
   * @return the evaluation context
   */
  private EvaluationContext createEvaluationContext(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext) {

    StandardEvaluationContext evalContext = new StandardEvaluationContext();

    // Get Spring Security authentication
    Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

    if (authentication != null) {
      SecurityExpressionRoot root = new SecurityExpressionRoot(authentication) {};
      evalContext.setRootObject(root);

      // Also make authentication available as a variable for direct access
      evalContext.setVariable("authentication", authentication);
      evalContext.setVariable("principal", authentication.getPrincipal());
    }

    // Add route and navigation context
    evalContext.setVariable("routeClass", routeClass);
    evalContext.setVariable("context", context);
    evalContext.setVariable("securityContext", securityContext);

    return evalContext;
  }
}
