package com.webforj.i18n;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.webforj.i18n.event.LocaleEvent;
import java.util.Locale;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class LocaleObserverRegistryTest {

  private LocaleObserverRegistry registry;

  @BeforeEach
  void setUp() {
    registry = LocaleObserverRegistry.getCurrent();
    registry.clear();
  }

  @AfterEach
  void tearDown() {
    registry.clear();
  }

  @Test
  void shouldReturnNonNullRegistryInstance() {
    LocaleObserverRegistry result = LocaleObserverRegistry.getCurrent();
    assertNotNull(result);
  }

  @Test
  void shouldAddObserverToRegistry() {
    LocaleObserver observer = mock(LocaleObserver.class);

    registry.register(observer);

    assertEquals(1, registry.getObserverCount());
  }

  @Test
  void shouldThrowExceptionWhenRegisteringNullObserver() {
    assertThrows(NullPointerException.class, () -> registry.register(null));
  }

  @Test
  void shouldRemoveObserverFromRegistry() {
    LocaleObserver observer = mock(LocaleObserver.class);
    registry.register(observer);

    registry.unregister(observer);

    assertEquals(0, registry.getObserverCount());
  }

  @Test
  void shouldHandleNullObserverGracefullyWhenUnregistering() {
    assertDoesNotThrow(() -> registry.unregister(null));
  }

  @Test
  void shouldNotifyAllRegisteredObserversOnLocaleChange() {
    LocaleObserver observer1 = mock(LocaleObserver.class);
    LocaleObserver observer2 = mock(LocaleObserver.class);
    registry.register(observer1);
    registry.register(observer2);
    Locale locale = Locale.FRENCH;

    registry.fireLocaleChange(locale);

    verify(observer1, times(1)).onLocaleChange(any(LocaleEvent.class));
    verify(observer2, times(1)).onLocaleChange(any(LocaleEvent.class));
  }

  @Test
  void shouldThrowExceptionWhenFiringNullLocale() {
    assertThrows(NullPointerException.class, () -> registry.fireLocaleChange(null));
  }

  @Test
  void shouldContinueNotifyingIfOneObserverThrowsException() {
    LocaleObserver observer1 = mock(LocaleObserver.class);
    LocaleObserver observer2 = mock(LocaleObserver.class);
    doThrow(new RuntimeException("Test exception")).when(observer1).onLocaleChange(any());

    registry.register(observer1);
    registry.register(observer2);

    registry.fireLocaleChange(Locale.GERMAN);

    verify(observer2, times(1)).onLocaleChange(any(LocaleEvent.class));
  }

  @Test
  void shouldRemoveAllObserversWhenClearing() {
    LocaleObserver observer1 = mock(LocaleObserver.class);
    LocaleObserver observer2 = mock(LocaleObserver.class);
    registry.register(observer1);
    registry.register(observer2);

    registry.clear();

    assertEquals(0, registry.getObserverCount());
  }

  @Test
  void shouldNotNotifyUnregisteredObservers() {
    LocaleObserver observer = mock(LocaleObserver.class);
    registry.register(observer);
    registry.unregister(observer);

    registry.fireLocaleChange(Locale.ENGLISH);

    verify(observer, never()).onLocaleChange(any());
  }
}
