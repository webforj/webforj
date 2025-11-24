package com.webforj.spring;

import com.webforj.component.Component;
import com.webforj.router.RouteRegistry;
import com.webforj.router.RouteRegistryProvider;
import com.webforj.router.annotation.Route;
import java.lang.System.Logger;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;

/**
 * Spring-based route registry provider that integrates webforJ routing with Spring's dependency
 * injection and bean lifecycle management.
 *
 * <p>
 * This provider is discovered via Java's {@link java.util.ServiceLoader} mechanism and is used when
 * a Spring application context is available. It retrieves {@code @Route} annotated components from
 * the Spring bean factory and registers them into the provided {@link RouteRegistry}, enabling full
 * Spring features including dependency injection.
 * </p>
 *
 * <p>
 * The provider supports both startup-time registration (via {@code @Routify} annotation) and
 * runtime package registration. When registering routes for packages not yet scanned, it delegates
 * to {@link ComponentRegistrar} to ensure the requested packages are registered as Spring beans
 * before adding routes to the {@link RouteRegistry}.
 * </p>
 *
 * <p>
 * Route components are registered with PROTOTYPE scope and LAZY initialization, ensuring that
 * instances are created only when routes are accessed and that each navigation creates a fresh
 * component instance.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
public class SpringRouteRegistryProvider implements RouteRegistryProvider {
  private static final Logger logger =
      System.getLogger(SpringRouteRegistryProvider.class.getName());

  @Override
  @SuppressWarnings("resource") // Context lifecycle is managed by Spring, not this method
  public void registerRoutes(String[] packages, RouteRegistry registry) {
    ApplicationContext context = ContextHolder.getContext();
    if (context == null) {
      logger.log(Logger.Level.DEBUG, "Spring context not available, registry will remain empty");
      return;
    }

    if (!(context instanceof ConfigurableApplicationContext)) {
      logger.log(Logger.Level.WARNING,
          "ApplicationContext is not ConfigurableApplicationContext, registry will remain empty");
      return;
    }

    ConfigurableApplicationContext configurableContext = (ConfigurableApplicationContext) context;
    ConfigurableListableBeanFactory beanFactory = configurableContext.getBeanFactory();

    // Ensure packages are scanned and registered
    ComponentRegistrar registrar = context.getBean(ComponentRegistrar.class);
    registrar.ensurePackagesRegistered((BeanDefinitionRegistry) beanFactory, packages);

    // Register routes from Spring beans into RouteRegistry
    try {
      String[] beanNames = beanFactory.getBeanNamesForAnnotation(Route.class);
      int registeredCount = 0;

      for (String beanName : beanNames) {
        BeanDefinition beanDefinition = beanFactory.getBeanDefinition(beanName);
        String className = beanDefinition.getBeanClassName();

        if (className == null || !isInPackages(className, packages)) {
          continue;
        }

        Class<?> clazz = Class.forName(className);
        if (Component.class.isAssignableFrom(clazz)) {
          @SuppressWarnings("unchecked")
          Class<? extends Component> componentClass = (Class<? extends Component>) clazz;
          registry.register(componentClass);
          registeredCount++;
          logger.log(Logger.Level.DEBUG, "Registered route: " + className);
        }
      }

      if (registeredCount > 0) {
        String packagesInfo = (packages == null || packages.length == 0) ? "all packages"
            : String.join(", ", packages);
        logger.log(Logger.Level.INFO,
            "Registered " + registeredCount + " route(s) from packages: " + packagesInfo);
      }
    } catch (Exception e) {
      logger.log(Logger.Level.ERROR,
          "Error registering routes: " + e.getMessage() + ". Returning registry with " +
          registry.getAvailableRouteEntires().size() + " routes.", e);
    }
  }

  private boolean isInPackages(String className, String[] packages) {
    if (packages == null || packages.length == 0) {
      return true;
    }

    for (String pkg : packages) {
      if (className.startsWith(pkg + ".") || className.equals(pkg)) {
        return true;
      }
    }

    return false;
  }
}
