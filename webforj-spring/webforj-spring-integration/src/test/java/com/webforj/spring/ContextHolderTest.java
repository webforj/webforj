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
    void shouldSetAndGetContext() {
      ContextHolder.setContext(applicationContext);
      assertEquals(applicationContext, ContextHolder.getContext());
    }

    @Test
    void shouldReturnNullWhenContextNotSetOrClearedBySettingNull() {
      // Initially null
      assertNull(ContextHolder.getContext());

      // Set context then clear with null
      ContextHolder.setContext(applicationContext);
      ContextHolder.setContext(null);
      assertNull(ContextHolder.getContext());
    }

    @Test
    void shouldOverwriteExistingContext() {
      ApplicationContext firstContext = mock(ApplicationContext.class);
      ApplicationContext secondContext = mock(ApplicationContext.class);

      ContextHolder.setContext(firstContext);
      ContextHolder.setContext(secondContext);
      assertEquals(secondContext, ContextHolder.getContext());
    }
  }
}
