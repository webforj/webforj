package com.webforj.component.layout.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mockStatic;

import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.annotation.Route;
import com.webforj.router.history.MemoryHistory;
import com.webforj.router.history.ParametersBag;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AppNavItemTest {

  MockedStatic<Router> mockedRouter;
  AppNavItem component;
  Router currentRouter;

  @BeforeEach
  void setUp() {
    component = new AppNavItem("test item");

    RouteRegistry registry = new RouteRegistry();
    registry.register(ParameterizedView.class);
    registry.register(View.class);

    Router router = new Router(registry, new MemoryHistory());
    mockedRouter = mockStatic(Router.class);
    mockedRouter.when(Router::getCurrent).thenReturn(router);
  }

  @AfterEach
  void tearDown() {
    mockedRouter.close();
  }

  @Nested
  class ConstructorsTest {

    @Test
    void shouldCreateNavItemWithTextAndPath() {
      String text = "Home";
      String path = "/home";

      AppNavItem item = new AppNavItem(text, path);

      assertEquals(text, item.getText());
      assertEquals(path, item.getPath());
      assertEquals(path, item.getFullPath());
    }

    @Test
    void shouldCreateNavItemWithTextPathAndPrefix() {
      String text = "Home";
      String path = "/home";
      Component prefix = new Element("div");

      AppNavItem item = new AppNavItem(text, path, prefix);

      assertEquals(text, item.getText());
      assertEquals(path, item.getPath());
      assertEquals(prefix, item.getPrefixComponent());
    }

    @Test
    void shouldCreateNavItemWithTextViewAndRouteParams() {
      String text = "Details";
      Class<? extends Component> view = ParameterizedView.class;
      ParametersBag params = ParametersBag.of(Map.of("id", "123"));

      AppNavItem item = new AppNavItem(text, view, params);

      assertEquals(text, item.getText());
      assertEquals("/test/123", item.getFullPath());
    }

    @Test
    void shouldCreateNavItemWithTextViewRouteParamsAndPrefix() {
      String text = "Details";
      Class<? extends Component> view = ParameterizedView.class;
      ParametersBag params = ParametersBag.of(Map.of("id", "123"));
      Component prefix = new Element("div");

      AppNavItem item = new AppNavItem(text, view, params, prefix);

      assertEquals(text, item.getText());
      assertEquals("/test/123", item.getFullPath());
      assertEquals(prefix, item.getPrefixComponent());
    }

    @Test
    void shouldCreateNavItemWithTextAndView() {
      String text = "Test View";
      Class<? extends Component> view = View.class;

      AppNavItem item = new AppNavItem(text, view);

      assertEquals(text, item.getText());
      assertEquals("/test", item.getFullPath());
    }

    @Test
    void shouldCreateNavItemWithOnlyText() {
      String text = "Simple Item";

      AppNavItem item = new AppNavItem(text);

      assertEquals(text, item.getText());
      assertNull(item.getPath());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(AppNavItem.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldUpdateHrefWhenPathChanges() {
      component.setPath("/test");
      assertEquals("/test", component.getPath());
      assertEquals("/test", component.getFullPath());
    }

    @Test
    void shouldUpdateHrefWhenQueryParamsChanges() {
      component.setPath("/test");
      Map<String, String> queryParams = Map.of("key", "value");
      component.setQueryParameters(ParametersBag.of(queryParams));

      assertEquals("/test?key=value", component.getFullPath());
    }

    @Test
    void shouldUpdateHrefBasedOnViewAndRouteParams() {
      Map<String, String> queryParams = Map.of("id", "value");
      component.setPath(ParameterizedView.class, ParametersBag.of(queryParams));
      assertEquals("/test/value", component.getFullPath());
    }

    @Test
    void shouldUpdateHrefBasedOnView() {
      component.setPath(View.class);
      assertEquals("/test", component.getFullPath());
    }
  }

  @Nested
  class PrefixSuffixApi {

    @Test
    void shouldSetPrefixAndSuffix() {
      Element prefix = new Element("spam");
      Element suffix = new Element("span");
      component.setPrefixComponent(prefix);
      component.setSuffixComponent(suffix);

      assertSame(prefix, component.getPrefixComponent());
      assertSame(suffix, component.getSuffixComponent());
    }

    @Test
    void shouldDestroyPreviousPrefixAndSuffix() {
      Element oldPrefix = new Element("spam");
      Element oldSuffix = new Element("span");
      component.setPrefixComponent(oldPrefix);
      component.setSuffixComponent(oldSuffix);

      Element newPrefix = new Element("spam");
      Element newSuffix = new Element("span");
      component.setPrefixComponent(newPrefix);
      component.setSuffixComponent(newSuffix);

      assertTrue(oldPrefix.isDestroyed());
      assertTrue(oldSuffix.isDestroyed());

      assertSame(newPrefix, component.getPrefixComponent());
      assertSame(newSuffix, component.getSuffixComponent());
    }

    @Test
    void shouldIgnoreSamePrefixAndSuffix() {
      Element prefix = new Element("spam");
      Element suffix = new Element("span");
      component.setPrefixComponent(prefix);
      component.setSuffixComponent(suffix);

      component.setPrefixComponent(prefix);
      component.setSuffixComponent(suffix);

      assertFalse(prefix.isDestroyed());
      assertFalse(suffix.isDestroyed());

      assertSame(prefix, component.getPrefixComponent());
      assertSame(suffix, component.getSuffixComponent());
    }
  }

  @Nested
  @DisplayName("Absolute URL Tests")
  class AbsoluteUrlTests {

    @Test
    void shouldHandleAbsoluteUrlWithoutQueryParams() {
      String absoluteUrl = "https://example.com/path";

      component.setPath(absoluteUrl);

      assertEquals(absoluteUrl, component.getFullPath());
      assertTrue(component.isRouterIgnore());
    }

    @Test
    void shouldHandleAbsoluteUrlWithQueryParams() {
      String absoluteUrl = "https://example.com/path";
      Map<String, String> queryParams = Map.of("key", "value");

      component.setPath(absoluteUrl);
      component.setQueryParameters(ParametersBag.of(queryParams));

      assertEquals("https://example.com/path?key=value", component.getFullPath());
      assertTrue(component.isRouterIgnore());
    }

    @Test
    void shouldMergeQueryParamsWithAbsoluteUrl() {
      String absoluteUrl = "https://example.com/path?existingKey=existingValue";
      Map<String, String> queryParams = Map.of("newKey", "newValue");

      component.setPath(absoluteUrl);
      component.setQueryParameters(ParametersBag.of(queryParams));

      assertEquals("https://example.com/path?existingKey=existingValue&newKey=newValue",
          component.getFullPath());
      assertTrue(component.isRouterIgnore());
    }

    @Test
    void shouldPreserveFragmentInAbsoluteUrl() {
      String absoluteUrl = "https://example.com/path#fragment";
      Map<String, String> queryParams = Map.of("key", "value");

      component.setPath(absoluteUrl);
      component.setQueryParameters(ParametersBag.of(queryParams));

      assertEquals("https://example.com/path?key=value#fragment", component.getFullPath());
      assertTrue(component.isRouterIgnore());
    }
  }

  @Route("/test/:id")
  static final class ParameterizedView extends ElementComposite {
  }

  @Route("/test")
  static final class View extends ElementComposite {
  }
}
