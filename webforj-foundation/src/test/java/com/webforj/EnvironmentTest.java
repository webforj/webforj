package com.webforj;

import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.error.ErrorHandler;
import com.webforj.error.GlobalErrorHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class EnvironmentTest {

  ErrorHandler specificHandler;
  ErrorHandler specificGlobalHandler;
  ErrorHandler globalHandler;


  @BeforeEach
  void setUp() {
    specificHandler = mock(IllegalArgumentExceptionErrorHandler.class);
    specificGlobalHandler = mock(WebforjGlobalErrorHandler.class);
    globalHandler = mock(GlobalErrorHandler.class);
  }

  @Test
  void shouldHandleErrorWithSpecificHandler() {
    try (MockedStatic<Environment> mockedEnvStatic =
        mockStatic(Environment.class, CALLS_REAL_METHODS)) {

      mockedEnvStatic.when(Environment::getGlobalErrorHandler).thenReturn(globalHandler);

      ServiceLoader<ErrorHandler> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn(List.of(specificHandler).iterator());

      Exception ex = new IllegalArgumentException();
      Environment.handleError(ex, 1, mockLoader);

      verify(specificHandler).onError(ex, true);
    }
  }

  @Test
  void shouldHandleErrorWithGlobalHandler() {
    try (MockedStatic<Environment> mockedEnvStatic =
        mockStatic(Environment.class, CALLS_REAL_METHODS)) {

      mockedEnvStatic.when(Environment::getGlobalErrorHandler).thenReturn(globalHandler);

      ServiceLoader<ErrorHandler> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn((new ArrayList<ErrorHandler>()).iterator());

      Exception ex = new IllegalArgumentException();
      Environment.handleError(ex, 1, mockLoader);

      verify(globalHandler).onError(ex, true);
    }
  }

  @Test
  void shouldHandleErrorWithSpecificGlobalHandler() {
    try (MockedStatic<Environment> mockedEnvStatic =
        mockStatic(Environment.class, CALLS_REAL_METHODS)) {

      mockedEnvStatic.when(Environment::getGlobalErrorHandler).thenReturn(globalHandler);

      ServiceLoader<ErrorHandler> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn(List.of(specificGlobalHandler).iterator());

      Exception ex = new IllegalArgumentException();
      Environment.handleError(ex, 1, mockLoader);

      verify(specificGlobalHandler).onError(ex, true);
    }
  }

  class IllegalArgumentExceptionErrorHandler implements ErrorHandler {
    @Override
    public void onError(Throwable throwable, boolean debug) {
      // no-op
    }
  }

  class WebforjGlobalErrorHandler implements ErrorHandler {
    @Override
    public void onError(Throwable throwable, boolean debug) {
      // no-op
    }
  }
}
