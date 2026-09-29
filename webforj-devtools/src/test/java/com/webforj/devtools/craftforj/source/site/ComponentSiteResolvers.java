package com.webforj.devtools.craftforj.source.site;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Creates resolvers for tests that run without an application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ComponentSiteResolvers {

  private ComponentSiteResolvers() {}

  /**
   * Creates a resolver that knows the given components.
   *
   * @param components supplies every component of the application
   * @param descendants finds the components a component holds
   * @return the resolver
   */
  public static ComponentSiteResolver create(Supplier<List<Component>> components,
      Function<Component, List<Component>> descendants) {
    return new ComponentSiteResolver(new SourceParserService(), components, descendants);
  }
}
