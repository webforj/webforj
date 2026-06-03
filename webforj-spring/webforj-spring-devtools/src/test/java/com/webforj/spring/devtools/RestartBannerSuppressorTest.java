package com.webforj.spring.devtools;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.Banner;
import org.springframework.boot.SpringApplication;

class RestartBannerSuppressorTest {

  private static final String SHOWN_PROPERTY = "webforj.devtools.banner-shown";

  @Mock
  private SpringApplication application;

  private AutoCloseable mocks;
  private RestartBannerSuppressor suppressor;

  @BeforeEach
  void setUp() {
    mocks = MockitoAnnotations.openMocks(this);
    System.clearProperty(SHOWN_PROPERTY);
    suppressor = new RestartBannerSuppressor(application, new String[0]);
  }

  @AfterEach
  void tearDown() throws Exception {
    System.clearProperty(SHOWN_PROPERTY);
    mocks.close();
  }

  @Test
  void shouldPrintBannerOnFirstStart() {
    suppressor.starting(null);

    verify(application, never()).setBannerMode(Banner.Mode.OFF);
  }

  @Test
  void shouldSuppressBannerWhenAlreadyShown() {
    System.setProperty(SHOWN_PROPERTY, "true");

    suppressor.starting(null);

    verify(application).setBannerMode(Banner.Mode.OFF);
  }

  @Test
  void shouldMarkBannerShownOnContextPrepared() {
    assertNull(System.getProperty(SHOWN_PROPERTY));

    suppressor.contextPrepared(null);

    assertNotNull(System.getProperty(SHOWN_PROPERTY));
  }
}
