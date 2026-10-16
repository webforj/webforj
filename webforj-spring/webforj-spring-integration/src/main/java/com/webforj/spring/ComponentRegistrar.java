package com.webforj.spring;

import com.webforj.annotation.Routify;
import com.webforj.router.RouteRegistry;
import com.webforj.router.annotation.Route;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.annotation.Annotation;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor;
import org.springframework.beans.factory.support.BeanNameGenerator;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.EnvironmentAware;
import org.springframework.context.annotation.AnnotationBeanNameGenerator;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.stereotype.Component;
import org.springframework.util.ClassUtils;
import org.springframework.util.ResourceUtils;

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
public class ComponentRegistrar implements BeanDefinitionRegistryPostProcessor, EnvironmentAware {
  static final String LIVE_RELOAD_ENABLED_KEY = "webforj.devtools.livereload.enabled";

  private final Logger logger = System.getLogger(ComponentRegistrar.class.getName());
  private static final BeanNameGenerator beanNameGenerator = AnnotationBeanNameGenerator.INSTANCE;
  private final Set<String> registeredPackages = ConcurrentHashMap.newKeySet();
  private final Map<String, Path> routeClassFiles = new HashMap<>();
  private final Map<String, FileTime> routeClassFileTimes = new HashMap<>();
  private final Set<String> untrackedBeanNames = new HashSet<>();

  private Environment environment;
  private ClassLoader beanClassLoader;

  /**
   * {@inheritDoc}
   */
  @Override
  public void setEnvironment(Environment environment) {
    this.environment = environment;
  }

  /**
   * Ensures the specified packages are scanned for {@code @Route} components and registered as
   * Spring beans if not already present.
   *
   * <p>
   * This method enables dynamic package registration at runtime, complementing the automatic
   * registration performed during application startup via {@code @Routify}. It is particularly
   * useful when routes need to be registered from packages not declared in the {@code @Routify}
   * annotation, such as when building a {@link RouteRegistry} for additional packages discovered at
   * runtime.
   * </p>
   *
   * <p>
   * The method is thread-safe. Packages already scanned will be skipped, unless live reload is
   * enabled, in which case every package is scanned again so classes written while the application
   * runs are picked up. All registered route components receive PROTOTYPE scope and LAZY
   * initialization.
   * </p>
   *
   * <p>
   * When live reload is enabled, the definition of a route found in a class folder is replaced only
   * when the modification time of its class file changed since the previous scan, and removed when
   * its class file is gone. A scan with no changed class file leaves every definition in place.
   * </p>
   *
   * @param registry the bean definition registry to register components in
   * @param packages the packages to scan for route components; null or empty array is a no-op
   */
  public synchronized void ensurePackagesRegistered(BeanDefinitionRegistry registry,
      String[] packages) {
    if (packages == null || packages.length == 0) {
      return;
    }

    boolean rescanEveryPackage = isLiveReloadEnabled();
    Set<String> packagesToScan = new HashSet<>();
    for (String pkg : packages) {
      if (rescanEveryPackage || !registeredPackages.contains(pkg)) {
        packagesToScan.add(pkg);
      }
    }

    if (!packagesToScan.isEmpty()) {
      if (rescanEveryPackage) {
        removeGoneRouteDefinitions(registry, packagesToScan);
      }

      ClassPathScanningCandidateComponentProvider scanner = createScanner();
      registerWebforjComponents(scanner, registry, Route.class, true, packagesToScan,
          rescanEveryPackage);
      registeredPackages.addAll(packagesToScan);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void postProcessBeanDefinitionRegistry(BeanDefinitionRegistry registry)
      throws BeansException {

    // First check if @Routify is defined to determine packages to scan
    Set<String> packagesToScan = getPackagesToScan(registry);

    // Only register components if @Routify is defined
    if (!packagesToScan.isEmpty()) {
      ClassPathScanningCandidateComponentProvider scanner = createScanner();
      registerWebforjComponents(scanner, registry, Route.class, true, packagesToScan, false);
      registerWebforjComponents(scanner, registry, Routify.class, false, packagesToScan, false);
      registeredPackages.addAll(packagesToScan);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory)
      throws BeansException {
    beanClassLoader = beanFactory.getBeanClassLoader();
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
   * @param registry the bean definition registry
   *
   * @return set of package names to scan, or empty set if {@code @Routify} is not present
   */
  private Set<String> getPackagesToScan(BeanDefinitionRegistry registry) {
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
   * @param replace whether an existing definition is replaced when its class file changed
   */
  private void registerWebforjComponents(ClassPathScanningCandidateComponentProvider scanner,
      BeanDefinitionRegistry registry, Class<? extends Annotation> annotationClass, boolean lazy,
      Set<String> packagesToScan, boolean replace) {

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
          registerWebforjComponent(registry, componentClass, lazy, replace);
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
   * Creates a bean definition with PROTOTYPE scope and the specified lazy initialization. An
   * existing definition with the same name is kept, unless {@code replace} is set and the class
   * file of the component changed since the previous scan.
   * </p>
   *
   * @param registry the bean definition registry
   * @param componentClass the component class to register
   * @param lazy whether the bean should be lazy-initialized
   * @param replace whether an existing definition is replaced when its class file changed
   */
  private void registerWebforjComponent(BeanDefinitionRegistry registry, Class<?> componentClass,
      boolean lazy, boolean replace) {

    BeanDefinition beanDefinition = BeanDefinitionBuilder.genericBeanDefinition(componentClass)
        .setScope(BeanDefinition.SCOPE_PROTOTYPE).setLazyInit(lazy).getBeanDefinition();

    String beanName = beanNameGenerator.generateBeanName(beanDefinition, registry);

    if (registry.containsBeanDefinition(beanName)) {
      if (!replace || !updateClassFileTime(beanName, componentClass.getName())) {
        return;
      }

      registry.removeBeanDefinition(beanName);
    }

    registry.registerBeanDefinition(beanName, beanDefinition);
    if (replace && !routeClassFiles.containsKey(beanName)) {
      addClassFile(beanName, resolveClassFileUrl(componentClass.getName()));
    }
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
    // Check for @Component and its stereotypes (@Service, @Repository, @Controller, etc.),
    // @Configuration classes, and @SpringBootApplication classes
    return AnnotationUtils.findAnnotation(clazz, Component.class) != null
        || AnnotationUtils.findAnnotation(clazz, Configuration.class) != null
        || AnnotationUtils.findAnnotation(clazz, SpringBootApplication.class) != null;
  }

  /**
   * Replaces the definition of a route when its class file changed since it was last checked.
   *
   * <p>
   * Applies only when live reload is enabled, the class carries {@code @Route}, is not managed by
   * Spring and the registry holds a definition for it. Otherwise nothing happens.
   * </p>
   *
   * @param registry the bean definition registry holding the route definition
   * @param routeClass the route class whose definition is updated
   */
  synchronized void updateRouteDefinition(BeanDefinitionRegistry registry, Class<?> routeClass) {
    if (!isLiveReloadEnabled() || !routeClass.isAnnotationPresent(Route.class)
        || isSpringManagedComponent(routeClass)) {
      return;
    }

    String beanName = beanNameGenerator.generateBeanName(
        BeanDefinitionBuilder.genericBeanDefinition(routeClass).getBeanDefinition(), registry);
    if (registry.containsBeanDefinition(beanName)) {
      registerWebforjComponent(registry, routeClass, true, true);
    }
  }

  /**
   * Checks if a package has been registered.
   *
   * @param packageName the package name to check
   * @return true if the package has been registered
   */
  boolean isPackageRegistered(String packageName) {
    return registeredPackages.contains(packageName);
  }

  /**
   * Gets the count of registered packages.
   *
   * @return the number of registered packages
   */
  int getRegisteredPackageCount() {
    return registeredPackages.size();
  }

  /**
   * Indicates whether a class can appear in a package after that package has been scanned.
   *
   * <p>
   * When live reload is off the result of a scan stays complete for the life of the application,
   * because the classes on the classpath cannot change, so an already scanned package is skipped.
   * When live reload is on the developer is editing the running application, a class compiled after
   * the scan would never reach the registry, so the skip is dropped and the package is scanned
   * again.
   * </p>
   *
   * @return {@code true} when live reload is enabled
   */
  private boolean isLiveReloadEnabled() {
    return environment != null
        && Boolean.parseBoolean(environment.getProperty(LIVE_RELOAD_ENABLED_KEY, "false"));
  }

  private void removeGoneRouteDefinitions(BeanDefinitionRegistry registry,
      Set<String> packagesToScan) {
    for (String beanName : registry.getBeanDefinitionNames()) {
      BeanDefinition beanDefinition = registry.getBeanDefinition(beanName);
      String className = beanDefinition.getBeanClassName();
      if (className != null && isInPackages(className, packagesToScan)
          && isGoneRoute(beanName, beanDefinition, className)) {
        registry.removeBeanDefinition(beanName);
        routeClassFiles.remove(beanName);
        routeClassFileTimes.remove(beanName);
        logger.log(System.Logger.Level.DEBUG,
            "Removed the bean definition of the route " + className + ", its class file is gone");
      }
    }
  }

  private boolean isGoneRoute(String beanName, BeanDefinition beanDefinition, String className) {
    Path classFile = routeClassFiles.get(beanName);
    if (classFile != null) {
      return !Files.exists(classFile);
    }

    if (untrackedBeanNames.contains(beanName)) {
      return false;
    }

    Class<?> componentClass = resolveBeanClass(beanDefinition, className);
    if (componentClass == null || !componentClass.isAnnotationPresent(Route.class)
        || getClassLoader() == null) {
      untrackedBeanNames.add(beanName);
      return false;
    }

    URL classFileUrl = resolveClassFileUrl(className);
    if (classFileUrl == null) {
      return true;
    }

    addClassFile(beanName, classFileUrl);
    return false;
  }

  /**
   * Records the modification time of the class file of a route definition.
   *
   * @param beanName the name of the route definition
   * @param className the name of the route class
   *
   * @return {@code true} when the time moved since the previous record
   */
  private boolean updateClassFileTime(String beanName, String className) {
    if (untrackedBeanNames.contains(beanName)) {
      return false;
    }

    Path classFile = routeClassFiles.get(beanName);
    if (classFile == null) {
      addClassFile(beanName, resolveClassFileUrl(className));
      return false;
    }

    FileTime time = readLastModifiedTime(classFile);
    if (time == null || time.equals(routeClassFileTimes.get(beanName))) {
      return false;
    }

    routeClassFileTimes.put(beanName, time);
    return true;
  }

  private void addClassFile(String beanName, URL classFileUrl) {
    if (classFileUrl == null) {
      return;
    }

    if (!ResourceUtils.URL_PROTOCOL_FILE.equals(classFileUrl.getProtocol())) {
      untrackedBeanNames.add(beanName);
      return;
    }

    try {
      Path classFile = ResourceUtils.getFile(classFileUrl).toPath();
      routeClassFiles.put(beanName, classFile);
      routeClassFileTimes.put(beanName, readLastModifiedTime(classFile));
    } catch (FileNotFoundException e) {
      untrackedBeanNames.add(beanName);
    }
  }

  private Class<?> resolveBeanClass(BeanDefinition beanDefinition, String className) {
    if (beanDefinition instanceof AbstractBeanDefinition definition && definition.hasBeanClass()) {
      return definition.getBeanClass();
    }

    try {
      return ClassUtils.forName(className, beanClassLoader);
    } catch (ClassNotFoundException | LinkageError e) {
      return null;
    }
  }

  private URL resolveClassFileUrl(String className) {
    ClassLoader loader = getClassLoader();
    return loader == null ? null : loader.getResource(className.replace('.', '/') + ".class");
  }

  private ClassLoader getClassLoader() {
    return beanClassLoader != null ? beanClassLoader : ClassUtils.getDefaultClassLoader();
  }

  private static boolean isInPackages(String className, Set<String> packages) {
    for (String pkg : packages) {
      if (className.startsWith(pkg + ".")) {
        return true;
      }
    }

    return false;
  }

  private static FileTime readLastModifiedTime(Path classFile) {
    try {
      return Files.getLastModifiedTime(classFile);
    } catch (IOException e) {
      return null;
    }
  }
}
