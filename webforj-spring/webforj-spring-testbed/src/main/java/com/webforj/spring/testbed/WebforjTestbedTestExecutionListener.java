package com.webforj.spring.testbed;

import com.webforj.spring.ContextInjector;
import org.springframework.core.Ordered;
import org.springframework.test.context.TestContext;
import org.springframework.test.context.TestExecutionListener;

/**
 * Points webforJ at the Spring context of the running test before every test method.
 *
 * <p>
 * Spring caches application contexts across test classes, so the context a test runs with may have
 * been created long before, and another test class with a different context may have taken the
 * place webforJ reads from in the meantime. The listener puts the right context back before each
 * test.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class WebforjTestbedTestExecutionListener implements TestExecutionListener, Ordered {

  /**
   * {@inheritDoc}
   */
  @Override
  public void prepareTestInstance(TestContext testContext) {
    expose(testContext);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void beforeTestMethod(TestContext testContext) {
    expose(testContext);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getOrder() {
    return LOWEST_PRECEDENCE;
  }

  private static void expose(TestContext testContext) {
    new ContextInjector().setApplicationContext(testContext.getApplicationContext());
  }
}
