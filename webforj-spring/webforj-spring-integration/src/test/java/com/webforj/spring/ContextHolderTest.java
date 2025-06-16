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
class ContextHolderTest {

  @Mock
  private ApplicationContext applicationContext;

  @BeforeEach
  void setUp() {
    ContextHolder.setContext(null);
  }

  @Nested
  class ContextManagement {

    @Test
    void shouldReturnSameContextWhenSet() {
      ContextHolder.setContext(applicationContext);
      assertEquals(applicationContext, ContextHolder.getContext());
    }

    @Test
    void shouldReturnNullWhenContextNotSet() {
      ApplicationContext result = ContextHolder.getContext();
      assertNull(result);
    }

    @Test
    void shouldReturnNullWhenContextSetToNull() {
      ContextHolder.setContext(applicationContext);
      ContextHolder.setContext(null);
      assertNull(ContextHolder.getContext());
    }

    @Test
    void shouldReturnNewContextWhenOverwritten() {
      ApplicationContext firstContext = mock(ApplicationContext.class);
      ApplicationContext secondContext = mock(ApplicationContext.class);

      ContextHolder.setContext(firstContext);
      assertEquals(firstContext, ContextHolder.getContext());
      ContextHolder.setContext(secondContext);
      assertEquals(secondContext, ContextHolder.getContext());
    }
  }

  @Nested
  class ThreadSafety {

    @Test
    void shouldHandleConcurrentAccess() throws InterruptedException {
      ApplicationContext context1 = mock(ApplicationContext.class);
      ApplicationContext context2 = mock(ApplicationContext.class);

      final ApplicationContext[] results = new ApplicationContext[2];
      final Exception[] exceptions = new Exception[2];

      Thread thread1 = new Thread(() -> {
        try {
          ContextHolder.setContext(context1);
          Thread.sleep(10);
          results[0] = ContextHolder.getContext();
        } catch (Exception e) {
          exceptions[0] = e;
        }
      });

      Thread thread2 = new Thread(() -> {
        try {
          ContextHolder.setContext(context2);
          Thread.sleep(10);
          results[1] = ContextHolder.getContext();
        } catch (Exception e) {
          exceptions[1] = e;
        }
      });

      thread1.start();
      thread2.start();

      thread1.join();
      thread2.join();

      assertNull(exceptions[0]);
      assertNull(exceptions[1]);
      assert results[0] != null || results[1] != null;
    }
  }
}
