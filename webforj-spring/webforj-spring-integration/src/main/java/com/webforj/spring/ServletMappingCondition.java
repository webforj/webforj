package com.webforj.spring;

import org.springframework.boot.context.properties.bind.Bindable;
import org.springframework.boot.context.properties.bind.Binder;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.core.env.Environment;
import org.springframework.core.type.AnnotatedTypeMetadata;

/**
 * Condition that evaluates the webforJ servlet mapping configuration.
 *
 * <p>
 * This condition checks if webforJ is configured with a root context mapping ({@code /*}). When
 * this condition is met, the application enables Spring MVC integration mode where
 * {@code DispatcherServlet} acts as the primary request handler and selectively forwards requests
 * to the webforJ servlet based on URL patterns.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public class ServletMappingCondition implements Condition {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
    Environment env = context.getEnvironment();

    // Use Binder to properly bind properties with relaxed naming
    // (supports both camelCase and kebab-case)
    Binder binder = Binder.get(env);
    SpringConfigurationProperties properties =
        binder.bind("webforj", Bindable.of(SpringConfigurationProperties.class))
            .orElseGet(SpringConfigurationProperties::new);

    String mapping = properties.getServletMapping();
    return isRootMapping(mapping);
  }

  /**
   * Returns {@code true} if {@code mapping} represents the root mapping.
   *
   * <p>
   * The mapping is considered a root mapping if it's {@code /*} or becomes empty after removing
   * trailing slashes and wildcards. This is controlled via the {@code webforj.servletMapping}
   * property value, which defaults to {@code /*}.
   * </p>
   *
   * @param mapping the mapping string to check
   * @return {@code true} if {@code mapping} is the root mapping, {@code false} otherwise
   */
  static boolean isRootMapping(String mapping) {
    if (mapping == null) {
      // Default is "/*"
      return true;
    }

    // Check if it's exactly "/*" or becomes empty after removing trailing slashes/wildcards
    String trimmed = mapping.trim();
    return "/*".equals(trimmed) || trimmed.replaceAll("(/\\**)?$", "").isEmpty();
  }
}
