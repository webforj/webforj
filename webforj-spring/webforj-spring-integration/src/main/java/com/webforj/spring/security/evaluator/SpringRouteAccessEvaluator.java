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
import org.springframework.security.authentication.AuthenticationTrustResolver;
import org.springframework.security.authentication.AuthenticationTrustResolverImpl;
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
 * @since 25.10
 */
public class SpringRouteAccessEvaluator implements RouteSecurityEvaluator {
  private static final Logger logger = System.getLogger(SpringRouteAccessEvaluator.class.getName());
  private final ExpressionParser parser = new SpelExpressionParser();
  private final AuthenticationTrustResolver trustResolver = new AuthenticationTrustResolverImpl();

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
    String code = annotation.code();

    // Evaluate the expression
    return evaluateExpression(expression, code, routeClass, context, securityContext, chain);
  }

  /**
   * Evaluates the access control expression using SpEL.
   *
   * @param expression the expression to evaluate
   * @param code the error code to use if access is denied
   * @param routeClass the route component class
   * @param context the navigation context
   * @param securityContext the security context
   * @param chain the evaluator chain
   * @return the access decision
   */
  private RouteAccessDecision evaluateExpression(String expression, String code,
      Class<?> routeClass, NavigationContext context, RouteSecurityContext securityContext,
      SecurityEvaluatorChain chain) {

    try {
      // Parse the expression
      Expression exp = parser.parseExpression(expression);

      // Create evaluation context
      EvaluationContext evalContext = createEvaluationContext(routeClass, context, securityContext);

      // Evaluate the expression
      Boolean result = exp.getValue(evalContext, Boolean.class);

      if (Boolean.TRUE.equals(result)) {
        // SpEL check passed - continue chain to allow other evaluators to run
        return chain.evaluate(routeClass, context, securityContext);
      } else {
        return RouteAccessDecision.deny(code);
      }
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error evaluating SpEL expression: {0}", expression, e);
      return RouteAccessDecision.deny(code);
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
      // Create and configure SecurityExpressionRoot
      SecurityExpressionRoot root = new SecurityExpressionRoot(authentication) {};
      root.setTrustResolver(trustResolver);
      root.setDefaultRolePrefix("ROLE_");

      evalContext.setRootObject(root);
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
