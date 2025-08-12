package com.webforj.spring;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.Config;
import com.webforj.Environment;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;

@ExtendWith(MockitoExtension.class)
class SpringConfigMergerTest {

  @Mock
  private Environment environment;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private Config config;

  private SpringConfigMerger merger;

  @BeforeEach
  void setUp() {
    merger = new SpringConfigMerger();
  }

  @Test
  void shouldMergeConfigWhenSpringContextAvailable() {
    try (MockedStatic<ContextHolder> contextHolder = mockStatic(ContextHolder.class)) {
      contextHolder.when(ContextHolder::getContext).thenReturn(applicationContext);
      when(applicationContext.getBean("webforjConfig", Config.class)).thenReturn(config);

      merger.onWillCreate(environment);

      verify(environment, times(1)).setConfig(config);
    }
  }

  @Test
  void shouldNotMergeConfigWhenSpringContextNotAvailable() {
    try (MockedStatic<ContextHolder> contextHolder = mockStatic(ContextHolder.class)) {
      contextHolder.when(ContextHolder::getContext).thenReturn(null);

      merger.onWillCreate(environment);

      verify(environment, never()).setConfig(any());
    }
  }

  @Test
  void shouldHandleExceptionWhenConfigBeanNotFound() {
    try (MockedStatic<ContextHolder> contextHolder = mockStatic(ContextHolder.class)) {
      contextHolder.when(ContextHolder::getContext).thenReturn(applicationContext);
      when(applicationContext.getBean("webforjConfig", Config.class))
          .thenThrow(new NoSuchBeanDefinitionException("webforjConfig"));

      merger.onWillCreate(environment);

      verify(environment, never()).setConfig(any());
    }
  }

  @Test
  void shouldRetrieveConfigBySpecificBeanName() {
    try (MockedStatic<ContextHolder> contextHolder = mockStatic(ContextHolder.class)) {
      contextHolder.when(ContextHolder::getContext).thenReturn(applicationContext);
      when(applicationContext.getBean("webforjConfig", Config.class)).thenReturn(config);

      merger.onWillCreate(environment);

      verify(applicationContext).getBean("webforjConfig", Config.class);
      verify(environment).setConfig(config);
    }
  }
}
