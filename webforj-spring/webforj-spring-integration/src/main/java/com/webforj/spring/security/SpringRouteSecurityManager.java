package com.webforj.spring.security;

import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.router.security.RouteSecurityConfiguration;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.AuthenticationRequiredEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import com.webforj.spring.security.annotation.RegisteredEvaluator;
import jakarta.annotation.PostConstruct;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

/**
 * Spring Security implementation of {@link AbstractRouteSecurityManager}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class SpringRouteSecurityManager extends AbstractRouteSecurityManager {
  private static final Logger logger = System.getLogger(SpringRouteSecurityManager.class.getName());
  private final SpringRouteSecurityContext securityContext = new SpringRouteSecurityContext();

  @Autowired
  private SpringRouteSecurityConfiguration configuration;

  @Autowired
  private ApplicationContext applicationContext;

  /**
   * Initializes the security manager by registering built-in and custom evaluators.
   *
   * <p>
   * Priority ranges:
   * <ul>
   * <li>0 - 9: Core framework evaluators</li>
   * <li>10+: Custom user evaluators</li>
   * </ul>
   * </p>
   */
  @PostConstruct
  public void initialize() {
    // Register built-in evaluators with default priorities

    // Always deny - highest priority
    registerEvaluator(new DenyAllEvaluator(), 1);

    // Public routes - bypass auth
    registerEvaluator(new AnonymousAccessEvaluator(), 2);

    // Check auth requirement
    registerEvaluator(new AuthenticationRequiredEvaluator(), 3);

    // Require authentication, allow all authenticated users
    registerEvaluator(new PermitAllEvaluator(), 4);

    // Check specific roles
    registerEvaluator(new RolesAllowedEvaluator(), 5);

    // Register Spring-specific evaluators
    try {
      RouteSecurityEvaluator routeAccessEvaluator =
          applicationContext.getBean("springRouteAccessEvaluator", RouteSecurityEvaluator.class);
      // SpEL expression evaluation
      registerEvaluator(routeAccessEvaluator, 6);
    } catch (Exception e) {
      logger.log(Level.DEBUG, "SpringRouteAccessEvaluator not found, skipping registration");
    }

    // Auto-discover and register custom evaluators annotated with @RegisteredEvaluator
    // These typically use priorities 10+ to run after core evaluators
    Map<String, Object> evaluators =
        applicationContext.getBeansWithAnnotation(RegisteredEvaluator.class);
    for (Map.Entry<String, Object> entry : evaluators.entrySet()) {
      Object bean = entry.getValue();
      if (bean instanceof RouteSecurityEvaluator routeSecurityEvaluator) {
        RegisteredEvaluator annotation = bean.getClass().getAnnotation(RegisteredEvaluator.class);
        int priority = annotation.priority();

        if (priority < 10) {
          logger.log(Level.WARNING,
              "Custom evaluator {0} uses priority {1} in core range (0 to 9). "
                  + "Consider using priority >= 10 for custom evaluators",
              routeSecurityEvaluator.getClass().getName(), priority);
        }

        registerEvaluator(routeSecurityEvaluator, priority);
        logger.log(Level.INFO, "Registered custom evaluator: {0} with priority {1}",
            routeSecurityEvaluator.getClass().getName(), priority);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteSecurityContext getSecurityContext() {
    return securityContext;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteSecurityConfiguration getConfiguration() {
    return configuration;
  }
}
