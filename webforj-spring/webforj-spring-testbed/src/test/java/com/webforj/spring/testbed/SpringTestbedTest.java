package com.webforj.spring.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.spring.ContextHolder;
import com.webforj.spring.testbed.fixture.GreetingService;
import com.webforj.spring.testbed.fixture.GreetingView;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.annotation.WebforjTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;

@SpringBootTest
@WebforjTest(routeClasses = GreetingView.class)
class SpringTestbedTest {

  @Autowired
  private ApplicationContext context;

  @Autowired
  private GreetingService service;

  @Test
  void shouldExposeTheSpringContextToTheEnvironment() {
    assertNotNull(context);
    assertSame(context, ContextHolder.getContext());
  }

  @Test
  void shouldSetUpTheWebforjTestbed(WebforjTestbed testContext) {
    assertTrue(WebforjTestbed.isPresent());
    assertNotNull(testContext.getFrame());
  }

  @Test
  void shouldResolveViewsThroughSpring(WebforjTestbed testContext) {
    GreetingView view = testContext.navigate(GreetingView.class);

    assertNotNull(view);
    assertTrue(view.isAttached());
    assertEquals(service.greet(), view.getButton().getText());
    assertEquals("Hello from Spring", view.getButton().getText());
  }
}
