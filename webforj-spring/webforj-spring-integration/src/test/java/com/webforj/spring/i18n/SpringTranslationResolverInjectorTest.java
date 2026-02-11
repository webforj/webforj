package com.webforj.spring.i18n;

import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.environment.ObjectTable;
import com.webforj.i18n.TranslationResolver;
import com.webforj.spring.ContextHolder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;

@ExtendWith(MockitoExtension.class)
class SpringTranslationResolverInjectorTest {

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private TranslationResolver translationResolver;

  @Mock
  private App mockApp;

  private SpringTranslationResolverInjector injector;

  @BeforeEach
  void setUp() {
    injector = new SpringTranslationResolverInjector();
  }

  @Nested
  class WhenContextExists {

    @Test
    void shouldInjectResolverWhenBeanExists() {
      when(applicationContext.getBean(TranslationResolver.class)).thenReturn(translationResolver);

      try (MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class);
          MockedStatic<ObjectTable> tableMock = mockStatic(ObjectTable.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        contextMock.when(ContextHolder::getContext).thenReturn(applicationContext);
        tableMock.when(() -> ObjectTable.contains(TranslationResolver.class.getName()))
            .thenReturn(false);

        injector.onWillRun(mockApp);

        appMock.verify(() -> App.setTranslationResolver(translationResolver));
      }
    }

    @Test
    void shouldNotInjectWhenResolverAlreadyConfigured() {
      try (MockedStatic<ObjectTable> tableMock = mockStatic(ObjectTable.class);
          MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        tableMock.when(() -> ObjectTable.contains(TranslationResolver.class.getName()))
            .thenReturn(true);

        injector.onWillRun(mockApp);

        // Return early without checking context or setting resolver
        contextMock.verify(ContextHolder::getContext, never());
        appMock.verify(() -> App.setTranslationResolver(translationResolver), never());
      }
    }

    @Test
    void shouldNotInjectWhenBeanNotFound() {
      when(applicationContext.getBean(TranslationResolver.class))
          .thenThrow(new NoSuchBeanDefinitionException(TranslationResolver.class));

      try (MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class);
          MockedStatic<ObjectTable> tableMock = mockStatic(ObjectTable.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        contextMock.when(ContextHolder::getContext).thenReturn(applicationContext);
        tableMock.when(() -> ObjectTable.contains(TranslationResolver.class.getName()))
            .thenReturn(false);

        injector.onWillRun(mockApp);

        appMock.verify(() -> App.setTranslationResolver(translationResolver), never());
      }
    }

    @Test
    void shouldNotInjectWhenBeanCreationFails() {
      when(applicationContext.getBean(TranslationResolver.class))
          .thenThrow(new RuntimeException("Bean wiring failed"));

      try (MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class);
          MockedStatic<ObjectTable> tableMock = mockStatic(ObjectTable.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        contextMock.when(ContextHolder::getContext).thenReturn(applicationContext);
        tableMock.when(() -> ObjectTable.contains(TranslationResolver.class.getName()))
            .thenReturn(false);

        injector.onWillRun(mockApp);

        appMock.verify(() -> App.setTranslationResolver(translationResolver), never());
      }
    }
  }

  @Nested
  class WhenContextIsNull {

    @Test
    void shouldNoOpWhenContextIsNull() {
      try (MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class);
          MockedStatic<ObjectTable> tableMock = mockStatic(ObjectTable.class);
          MockedStatic<App> appMock = mockStatic(App.class)) {

        contextMock.when(ContextHolder::getContext).thenReturn(null);
        tableMock.when(() -> ObjectTable.contains(TranslationResolver.class.getName()))
            .thenReturn(false);

        injector.onWillRun(mockApp);

        appMock.verify(() -> App.setTranslationResolver(translationResolver), never());
      }
    }
  }
}
