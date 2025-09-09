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
 * @since 25.04
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
   * <li>0.1-0.2: Security bypass evaluators (DenyAll, AnonymousAccess)</li>
   * <li>0.3-0.5: Authentication/authorization evaluators</li>
   * <li>0.6-0.9: Framework-specific evaluators (SpEL, etc.)</li>
   * <li>1.0+: Custom user evaluators</li>
   * </ul>
   * </p>
   */
  @PostConstruct
  public void initialize() {
    // Register built-in evaluators with default priorities
    registerEvaluator(new DenyAllEvaluator(), 0.1); // Always deny - highest priority
    registerEvaluator(new AnonymousAccessEvaluator(), 0.2); // Public routes - bypass auth
    registerEvaluator(new AuthenticationRequiredEvaluator(), 0.3); // Check auth requirement
    registerEvaluator(new PermitAllEvaluator(), 0.4); // Allow authenticated users
    registerEvaluator(new RolesAllowedEvaluator(), 0.5); // Check specific roles

    // Register Spring-specific evaluators (0.6-0.9 range)
    try {
      RouteSecurityEvaluator routeAccessEvaluator =
          applicationContext.getBean("springRouteAccessEvaluator", RouteSecurityEvaluator.class);
      registerEvaluator(routeAccessEvaluator, 0.6); // SpEL expression evaluation
    } catch (Exception e) {
      logger.log(Level.DEBUG, "SpringRouteAccessEvaluator not found, skipping registration");
    }

    // Auto-discover and register custom evaluators annotated with @RegisteredEvaluator
    // These typically use priorities 1.0+ to run after core evaluators
    Map<String, Object> evaluators =
        applicationContext.getBeansWithAnnotation(RegisteredEvaluator.class);
    for (Map.Entry<String, Object> entry : evaluators.entrySet()) {
      Object bean = entry.getValue();
      if (bean instanceof RouteSecurityEvaluator) {
        RegisteredEvaluator annotation = bean.getClass().getAnnotation(RegisteredEvaluator.class);
        double priority = annotation.priority();

        if (priority <= 0.0) {
          logger.log(Level.WARNING,
              "Skipping evaluator {0} with invalid priority {1}. Priority must be greater than 0",
              bean.getClass().getName(), priority);
          continue;
        }

        if (priority < 1.0) {
          logger.log(Level.WARNING,
              "Custom evaluator {0} uses priority {1} in core range (0.1-0.9). "
                  + "Consider using priority >= 1.0 for custom evaluators",
              bean.getClass().getName(), priority);
        }

        registerEvaluator((RouteSecurityEvaluator) bean, priority);
        logger.log(Level.INFO, "Registered custom evaluator: {0} with priority {1}",
            bean.getClass().getName(), priority);
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
