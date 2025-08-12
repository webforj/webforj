package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;

@ExtendWith(MockitoExtension.class)
class ContextInjectorTest {

  @Mock
  private ApplicationContext applicationContext;

  private ContextInjector contextInjector;

  @BeforeEach
  void setUp() {
    contextInjector = new ContextInjector();
    ContextHolder.setContext(null);
  }

  @Nested
  class ApplicationContextInjection {

    @Test
    void shouldInjectApplicationContextIntoContextHolder() {
      contextInjector.setApplicationContext(applicationContext);
      assertEquals(applicationContext, ContextHolder.getContext());
    }

    @Test
    void shouldSetNullInContextHolderWhenNullApplicationContextProvided() {
      ContextHolder.setContext(applicationContext);
      contextInjector.setApplicationContext(null);
      assertNull(ContextHolder.getContext());
    }

    @Test
    void shouldUpdateContextHolderWhenApplicationContextOverwritten() {
      ApplicationContext firstContext = mock(ApplicationContext.class);
      ApplicationContext secondContext = mock(ApplicationContext.class);

      contextInjector.setApplicationContext(firstContext);
      assertEquals(firstContext, ContextHolder.getContext());
      contextInjector.setApplicationContext(secondContext);
      assertEquals(secondContext, ContextHolder.getContext());
    }
  }

  @Nested
  class Cleanup {

    @Test
    void shouldClearContextHolderOnDestroy() throws Exception {
      contextInjector.setApplicationContext(applicationContext);
      assertEquals(applicationContext, ContextHolder.getContext());

      contextInjector.destroy();
      assertNull(ContextHolder.getContext());
    }
  }
}
