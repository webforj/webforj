package com.webforj.spring;

import com.webforj.annotation.Routify;
import com.webforj.router.annotation.Route;
import java.lang.System.Logger;
import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor;
import org.springframework.beans.factory.support.BeanNameGenerator;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.AnnotationBeanNameGenerator;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.stereotype.Component;

/**
 * Automatically registers webforj components as Spring beans with appropriate scoping and lazy
 * loading configuration.
 *
 * <p>
 * This registrar is conditionally created only when auto component registration is enabled via
 * {@code webforj.auto-register-components=true} (default). Once created, it scans for webforj
 * components when the {@code @Routify} annotation is present on the main Spring Boot application
 * class, and registers them as Spring beans:
 * </p>
 * <ul>
 * <li><strong>{@code @Route}</strong> - Registered as PROTOTYPE and LAZY (created only when route
 * is accessed)</li>
 * <li><strong>{@code @Routify}</strong> - Registered as PROTOTYPE and EAGER (needed for routing
 * setup)</li>
 * </ul>
 *
 * <p>
 * The registrar respects the {@code packages} attribute of {@code @Routify} to determine which
 * packages to scan. If no packages are specified, it defaults to the Spring Boot application's
 * package. Classes already managed by Spring through stereotype annotations are automatically
 * skipped to avoid registration conflicts.
 * </p>
 *
 * <p>
 * Bean names are generated using Spring's {@code AnnotationBeanNameGenerator} to ensure consistency
 * with Spring's own component scanning conventions.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@Component
public class ComponentRegistrar implements BeanDefinitionRegistryPostProcessor {
  private final Logger logger = System.getLogger(ComponentRegistrar.class.getName());
  private final BeanNameGenerator beanNameGenerator = AnnotationBeanNameGenerator.INSTANCE;

  /**
   * {@inheritDoc}
   */
  @Override
  public void postProcessBeanDefinitionRegistry(BeanDefinitionRegistry registry)
      throws BeansException {

    ClassPathScanningCandidateComponentProvider scanner = createScanner();

    // First check if @Routify is defined to determine packages to scan
    Set<String> packagesToScan = getPackagesToScan(scanner, registry);

    // Only register components if @Routify is defined
    if (!packagesToScan.isEmpty()) {
      registerWebforjComponents(scanner, registry, Route.class, true, packagesToScan);
      registerWebforjComponents(scanner, registry, Routify.class, false, packagesToScan);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory)
      throws BeansException {
    // No additional post-processing needed
  }

  /**
   * Determines which packages should be scanned for webforj components by examining
   * {@code @Routify} annotation on the main Spring Boot application class.
   *
   * <p>
   * Routes are only scanned when {@code @Routify} annotation is present on the main Spring Boot
   * application class. If {@code @Routify} is not found, an empty set is returned, preventing any
   * route scanning.
   * </p>
   *
   * @param scanner the component scanner
   * @param registry the bean definition registry
   *
   * @return set of package names to scan, or empty set if {@code @Routify} is not present
   */
  private Set<String> getPackagesToScan(ClassPathScanningCandidateComponentProvider scanner,
      BeanDefinitionRegistry registry) {
    Set<String> packagesToScan = new HashSet<>();

    try {
      Class<?> springBootApp = findSpringBootApplicationClass(registry);

      if (springBootApp != null && springBootApp.isAnnotationPresent(Routify.class)) {
        String[] packages = springBootApp.getAnnotation(Routify.class).packages();

        if (packages.length > 0) {
          packagesToScan.addAll(Arrays.asList(packages));
        } else {
          // If no packages specified, use the application class's package
          packagesToScan.add(springBootApp.getPackage().getName());
        }
      }
    } catch (Exception e) {
      logger.log(System.Logger.Level.WARNING,
          "Could not determine packages from @Routify annotations: " + e.getMessage(), e);
    }

    // If no @Routify found, don't scan for routes
    // packagesToScan will remain empty, preventing any route scanning

    return packagesToScan;
  }

  /**
   * Finds the main Spring Boot application class in the registry.
   *
   * <p>
   * Searches through all bean definitions to locate the class annotated with
   * {@code @SpringBootApplication}. This is where {@code @Routify} annotation should be defined.
   * </p>
   *
   * @param registry the bean definition registry
   * @return the Spring Boot application class, or null if not found
   */
  private Class<?> findSpringBootApplicationClass(BeanDefinitionRegistry registry) {
    try {
      String[] beanNames = registry.getBeanDefinitionNames();

      for (String beanName : beanNames) {
        BeanDefinition beanDefinition = registry.getBeanDefinition(beanName);
        String className = beanDefinition.getBeanClassName();

        if (className != null) {
          Class<?> clazz = Class.forName(className);
          if (clazz.isAnnotationPresent(SpringBootApplication.class)) {
            return clazz;
          }
        }
      }
    } catch (Exception e) {
      logger.log(System.Logger.Level.WARNING,
          "Could not find Spring Boot application class: " + e.getMessage(), e);
    }

    return null;
  }

  /**
   * Creates a component scanner that excludes default Spring stereotype annotations to avoid
   * conflicts with manually registered beans.
   *
   * <p>
   * The scanner is configured to only detect {@code @Route} and {@code @Routify} annotations,
   * ignoring standard Spring stereotypes like {@code @Component}, {@code @Service}, etc.
   * </p>
   *
   * @return configured scanner
   */
  private ClassPathScanningCandidateComponentProvider createScanner() {
    ClassPathScanningCandidateComponentProvider scanner =
        new ClassPathScanningCandidateComponentProvider(false);

    // Only include classes we explicitly want to register
    scanner.addIncludeFilter(new AnnotationTypeFilter(Route.class));
    scanner.addIncludeFilter(new AnnotationTypeFilter(Routify.class));

    return scanner;
  }

  /**
   * Registers webforj components with the specified annotation as Spring beans.
   *
   * <p>
   * Scans the specified packages for classes with the given annotation and registers them as Spring
   * beans with PROTOTYPE scope. Classes already managed by Spring are skipped to avoid conflicts.
   * </p>
   *
   * @param scanner the component scanner
   * @param registry the bean definition registry
   * @param annotationClass the annotation class to scan for
   * @param lazy whether the beans should be lazy-initialized
   * @param packagesToScan the packages to scan for components
   */
  private void registerWebforjComponents(ClassPathScanningCandidateComponentProvider scanner,
      BeanDefinitionRegistry registry, Class<? extends Annotation> annotationClass, boolean lazy,
      Set<String> packagesToScan) {

    try {
      Set<BeanDefinition> allCandidates = new HashSet<>();

      // Scan each specified package
      for (String packageToScan : packagesToScan) {
        Set<BeanDefinition> candidates = scanner.findCandidateComponents(packageToScan);
        allCandidates.addAll(candidates);
      }

      for (BeanDefinition candidate : allCandidates) {
        String className = candidate.getBeanClassName();

        if (className == null) {
          continue;
        }

        Class<?> componentClass = Class.forName(className);

        // Skip if already a Spring-managed component
        if (isSpringManagedComponent(componentClass)) {
          continue;
        }

        // Only register if it has the specific annotation we're looking for
        if (componentClass.isAnnotationPresent(annotationClass)) {
          registerWebforjComponent(registry, componentClass, lazy);
        }
      }
    } catch (Exception e) {
      // Log error but don't fail application startup
      logger.log(System.Logger.Level.WARNING,
          "Could not register webforj components for annotation " + annotationClass.getName() + ": "
              + e.getMessage(),
          e);
    }
  }

  /**
   * Registers a single webforj component as a Spring bean.
   *
   * <p>
   * Creates a bean definition with PROTOTYPE scope and the specified lazy initialization. Skips
   * registration if a bean with the same name already exists.
   * </p>
   *
   * @param registry the bean definition registry
   * @param componentClass the component class to register
   * @param lazy whether the bean should be lazy-initialized
   */
  private void registerWebforjComponent(BeanDefinitionRegistry registry, Class<?> componentClass,
      boolean lazy) {

    BeanDefinition beanDefinition = BeanDefinitionBuilder.genericBeanDefinition(componentClass)
        .setScope(BeanDefinition.SCOPE_PROTOTYPE).setLazyInit(lazy).getBeanDefinition();

    String beanName = beanNameGenerator.generateBeanName(beanDefinition, registry);

    if (registry.containsBeanDefinition(beanName)) {
      // Bean already registered, skip
      return;
    }

    registry.registerBeanDefinition(beanName, beanDefinition);
  }

  /**
   * Checks if a class is already managed by Spring through any Spring management annotations.
   *
   * <p>
   * Prevents duplicate registration by detecting classes that are already Spring-managed through:
   * <ul>
   * <li>{@code @Component} and its stereotypes ({@code @Service}, {@code @Repository}, etc.)</li>
   * <li>{@code @Configuration} classes</li>
   * <li>{@code @SpringBootApplication} classes</li>
   * </ul>
   * Uses Spring's {@code AnnotationUtils} to detect both direct and meta-annotations.
   * </p>
   *
   * @param clazz the class to check
   * @return true if the class has Spring management annotations
   */
  boolean isSpringManagedComponent(Class<?> clazz) {
    // Check for @Component and its stereotypes (@Service, @Repository, @Controller, etc.)
    if (AnnotationUtils.findAnnotation(clazz, Component.class) != null) {
      return true;
    }

    // Check for @Configuration classes
    if (AnnotationUtils.findAnnotation(clazz, Configuration.class) != null) {
      return true;
    }

    // Check for @SpringBootApplication classes
    if (AnnotationUtils.findAnnotation(clazz, SpringBootApplication.class) != null) {
      return true;
    }

    return false;
  }
}
