package com.webforj.spring.mcp;

import com.webforj.component.Component;
import com.webforj.mcp.McpAppContribution;
import com.webforj.mcp.McpAppOptions;
import com.webforj.router.RouteRegistry;
import com.webforj.router.annotation.Route;
import com.webforj.servlet.WebforjServlet;
import com.webforj.spring.SpringConfigurationProperties;
import io.modelcontextprotocol.server.McpServerFeatures.SyncResourceSpecification;
import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification;
import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification.Builder;
import java.lang.System.Logger;
import java.util.List;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.ServletContextInitializer;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;

/**
 * Publishes the webforJ contribution into the MCP server Spring AI runs.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoConfiguration
@ConditionalOnClass({Builder.class, McpAppContribution.class})
@EnableConfigurationProperties(SpringConfigurationProperties.class)
public class McpAppsAutoConfiguration {

  private static final Logger logger = System.getLogger(McpAppsAutoConfiguration.class.getName());

  /**
   * Assembles the contribution of the application from its route beans.
   *
   * @param beanFactory the bean factory holding the route definitions
   * @param properties the webforJ configuration of the application
   * @param servletRegistration the registration of the webforJ servlet
   * @return the contribution
   */
  @Bean
  McpAppContribution webforjMcpAppContribution(ConfigurableListableBeanFactory beanFactory,
      SpringConfigurationProperties properties,
      ServletRegistrationBean<WebforjServlet> servletRegistration) {
    String mapping = servletRegistration.getUrlMappings().stream().findFirst().orElse("/*");
    McpAppContribution contribution =
        McpAppContribution.ofRegistry(routeRegistry(beanFactory), mapping);
    contribution.getOrigin().configure(properties.getOrigin());

    return contribution;
  }

  /**
   * Publishes the tools of the contribution into the server Spring AI runs.
   *
   * @param contribution the contribution of the application
   * @return the tool specifications, empty when the application marks no view
   */
  @Bean
  List<SyncToolSpecification> webforjMcpTools(McpAppContribution contribution) {
    List<SyncToolSpecification> tools = contribution.getToolSpecifications();
    logger.log(Logger.Level.INFO,
        () -> "webforJ published " + tools.size() + " tools into the Spring AI server");

    return tools;
  }

  /**
   * Publishes the resources of the contribution into the server Spring AI runs.
   *
   * @param contribution the contribution of the application
   * @return the resource specifications
   */
  @Bean
  List<SyncResourceSpecification> webforjMcpResources(McpAppContribution contribution) {
    return contribution.getResourceSpecifications();
  }

  /**
   * Installs into the deployment everything a host needs to embed the application.
   *
   * @param contribution the contribution of the application
   * @param properties the webforJ configuration of the application
   * @return the initializer running the installation
   */
  @Bean
  ServletContextInitializer webforjMcpAppInstaller(McpAppContribution contribution,
      SpringConfigurationProperties properties) {
    return context -> contribution.install(context, deploymentOptions(properties));
  }

  private static McpAppOptions deploymentOptions(SpringConfigurationProperties properties) {
    List<String> allowedOrigins = properties.getMcp().getAllowedOrigins();

    return new McpAppOptions().setOrigin(properties.getOrigin())
        .setAllowedOrigins(allowedOrigins == null ? List.of() : allowedOrigins)
        .setComponents(properties.getComponents());
  }

  @SuppressWarnings("unchecked")
  private static RouteRegistry routeRegistry(ConfigurableListableBeanFactory beanFactory) {
    RouteRegistry registry = new RouteRegistry();

    for (String beanName : beanFactory.getBeanNamesForAnnotation(Route.class)) {
      BeanDefinition definition = beanFactory.getBeanDefinition(beanName);
      String className = definition.getBeanClassName();
      if (className == null) {
        continue;
      }

      try {
        Class<?> clazz =
            Class.forName(className, false, McpAppsAutoConfiguration.class.getClassLoader());
        if (Component.class.isAssignableFrom(clazz)) {
          registry.register((Class<? extends Component>) clazz);
        }
      } catch (ClassNotFoundException e) {
        logger.log(Logger.Level.WARNING,
            "Route bean class not loadable, skipped from the MCP contribution: " + className);
      }
    }

    return registry;
  }
}
