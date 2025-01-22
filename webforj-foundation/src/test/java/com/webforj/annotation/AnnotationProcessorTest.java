package com.webforj.annotation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjObjectTable;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.startup.type.BBjException;
import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.ProfileDescriptor;
import com.webforj.exceptions.WebforjException;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AnnotationProcessorTest {
  MockedStatic<Page> mockedPage;
  MockedStatic<Environment> mockedEnvironment;
  Page page;
  Environment environment;
  BBjAPI api;
  BBjObjectTable objectTable;
  BBjWebManager webManager;
  Config config;

  @BeforeEach
  void setup() throws BBjException {
    mockedPage = mockStatic(Page.class);
    mockedEnvironment = mockStatic(Environment.class);
    page = mock(Page.class);
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    objectTable = mock(BBjObjectTable.class);
    webManager = mock(BBjWebManager.class);
    config = mock(Config.class);

    mockedPage.when(Page::getCurrent).thenReturn(page);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
    when(environment.getBBjAPI()).thenReturn(api);
    when(api.getObjectTable()).thenReturn(objectTable);
    when(api.getWebManager()).thenReturn(webManager);
    when(webManager.getUrl()).thenReturn("http://localhost");
    when(environment.getConfig()).thenReturn(config);
    when(config.hasPath("webforj.assetsDir")).thenReturn(false);
  }

  @AfterEach
  void tearDown() {
    mockedPage.close();
    mockedEnvironment.close();
  }

  @Test
  void shouldProcessAppTitle() {
    @AppTitle(value = "Test App", format = "{BrowserTitle} - Generated")
    class MockAppClass extends App {
      @Override
      public void run() throws WebforjException {
        // pass
      }
    }

    AnnotationProcessor processor = new AnnotationProcessor();
    MockAppClass mockAppClass = new MockAppClass();

    processor.processAppAnnotations(mockAppClass);
    verify(page).setTitle("Test App", "{BrowserTitle} - Generated");
  }

  @Test
  void shouldProcessAppProfileMeta() {
    // @formatter:off
    @AppProfile(
        name = "my-app-name",
        shortName = "my-app-short-name",
        description = "my-app-description",
        themeColor = "#000000",
        backgroundColor = "#000000",
        viewport = "custom-viewport",
        defaultIcon = @AppProfile.DefaultIcon("http://localhost/static/icon.png")
    )
    // @formatter:on
    class MockAppClass extends App {
      @Override
      public void run() throws WebforjException {
        // pass
      }
    }

    AnnotationProcessor processor = new AnnotationProcessor();
    MockAppClass mockAppClass = new MockAppClass();

    ProfileDescriptor descriptor = processor.processAppProfile(mockAppClass);
    assertEquals("my-app-description", descriptor.getDescription());
    verify(page).setMeta("apple-mobile-web-app-title", "my-app-name");
    verify(page).setTitle("my-app-name");
    verify(page).setMeta("theme-color", "#000000");
    verify(page).setMeta("background-color", "#000000");
    verify(page).setMeta("viewport", "custom-viewport");
    verify(page).addLink(eq("http://localhost/static/icon.png"), eq(true),
        eq(Map.of("rel", "shortcut icon", "type", "image/png")));
  }

  @Test
  void shouldProcessScreenshots() {
    @AppProfile(name = "my-app-name", shortName = "my-app-short-name",
        screenshots = {
            @AppProfile.Screenshot(src = "http://localhost/static/screenshot1.png",
                sizes = "640x480"),
            @AppProfile.Screenshot(src = "http://localhost/static/screenshot2.png",
                sizes = "1280x720")})
    class MockAppClass extends App {
      @Override
      public void run() throws WebforjException {
        // pass
      }
    }

    AnnotationProcessor processor = new AnnotationProcessor();
    MockAppClass mockAppClass = new MockAppClass();

    ProfileDescriptor descriptor = processor.processAppProfile(mockAppClass);
    assertEquals(2, descriptor.getScreenshots().size());
    assertEquals("http://localhost/static/screenshot1.png",
        descriptor.getScreenshots().get(0).getSrc());
    assertEquals("640x480", descriptor.getScreenshots().get(0).getSizes());
    assertEquals("http://localhost/static/screenshot2.png",
        descriptor.getScreenshots().get(1).getSrc());
    assertEquals("1280x720", descriptor.getScreenshots().get(1).getSizes());
  }

  @Test
  void shouldProcessIcons() {
    @AppProfile(name = "my-app-name", shortName = "my-app-short-name",
        defaultIcon = @AppProfile.DefaultIcon(value = "http://localhost/static/default-icon.png",
            sizes = {}),
        icons = {@AppProfile.Icon(src = "http://localhost/static/icon1.png", sizes = "64x64"),
            @AppProfile.Icon(src = "http://localhost/static/icon2.png", sizes = "128x128")})
    class MockAppClass extends App {
      @Override
      public void run() throws WebforjException {
        // pass
      }
    }

    AnnotationProcessor processor = new AnnotationProcessor();
    MockAppClass mockAppClass = new MockAppClass();

    ProfileDescriptor descriptor = processor.processAppProfile(mockAppClass);
    assertEquals(2, descriptor.getIcons().size());
    assertEquals("http://localhost/static/icon1.png", descriptor.getIcons().get(0).getSrc());
    assertEquals("64x64", descriptor.getIcons().get(0).getSizes());
    assertEquals("http://localhost/static/icon2.png", descriptor.getIcons().get(1).getSrc());
    assertEquals("128x128", descriptor.getIcons().get(1).getSizes());
  }

  @Test
  void shouldProcessDefaultIcon() {
    @AppProfile(name = "my-app-name", shortName = "my-app-short-name",
        defaultIcon = @AppProfile.DefaultIcon("http://localhost/static/default-icon.png"))
    class MockAppClass extends App {
      @Override
      public void run() throws WebforjException {
        // pass
      }
    }

    AnnotationProcessor processor = new AnnotationProcessor();
    MockAppClass mockAppClass = new MockAppClass();

    ProfileDescriptor descriptor = processor.processAppProfile(mockAppClass);
    assertEquals("http://localhost/static/default-icon-144x144.png",
        descriptor.getIcons().get(0).getSrc());
    assertEquals("144x144", descriptor.getIcons().get(0).getSizes());

    assertEquals("http://localhost/static/default-icon-192x192.png",
        descriptor.getIcons().get(1).getSrc());
    assertEquals("192x192", descriptor.getIcons().get(1).getSizes());

    assertEquals("http://localhost/static/default-icon-512x512.png",
        descriptor.getIcons().get(2).getSrc());
    assertEquals("512x512", descriptor.getIcons().get(2).getSizes());
  }
}
