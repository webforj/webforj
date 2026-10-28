package com.webforj.spring.testbed;

import com.webforj.spring.ContextInjector;
import java.util.List;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.test.context.ContextConfigurationAttributes;
import org.springframework.test.context.ContextCustomizer;
import org.springframework.test.context.ContextCustomizerFactory;
import org.springframework.test.context.MergedContextConfiguration;

/**
 * Exposes the Spring test context to webforJ.
 *
 * <p>
 * The factory contributes a customizer to every Spring test context so that views and components
 * created inside a {@link com.webforj.testbed.WebforjTestbed} resolve through that context. In a
 * full Spring Boot test the auto configuration already exposes the context, and the customizer only
 * covers test contexts that run without it.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class WebforjTestbedContextCustomizerFactory implements ContextCustomizerFactory {

  @Override
  public ContextCustomizer createContextCustomizer(Class<?> testClass,
      List<ContextConfigurationAttributes> configAttributes) {
    return new WebforjTestbedCustomizer();
  }

  static final class WebforjTestbedCustomizer implements ContextCustomizer {

    @Override
    public void customizeContext(ConfigurableApplicationContext context,
        MergedContextConfiguration mergedConfig) {
      ContextInjector injector = new ContextInjector();
      injector.setApplicationContext(context);
      context.getBeanFactory().registerSingleton("webforjTestingContextInjector", injector);
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof WebforjTestbedCustomizer;
    }

    @Override
    public int hashCode() {
      return WebforjTestbedCustomizer.class.hashCode();
    }
  }
}
